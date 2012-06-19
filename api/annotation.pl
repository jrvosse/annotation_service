:- module(annotation_api, []). % use http api for access

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(graph_version)).
:- use_module(user(user_db)).

:- setting(login, boolean, true, 'Require login').
:- setting(user_restrict, boolean, false,
	   'When set to true only own annotations are shown.').

/***************************************************
* http handlers
***************************************************/

:- http_handler(cliopatria(api/annotation/add),    http_add_annotation, []).
:- http_handler(cliopatria(api/annotation/remove), http_remove_annotation, []).
:- http_handler(cliopatria(api/annotation/get),	   http_get_annotation, []).

%%	http_add_annotation(+Request)
%
%	Web service to add resource annotations

http_add_annotation(Request) :-
	http_parameters(Request,
		[ target(TargetURI,
		     [uri,
		      description('URI of the object that is annotated')
		     ]),
		  field(FieldURI,
			[uri,
			 description('URI of the annotation field')
			]),
		  body(Body0,
		       [json_rdf_object,
			description('Body of the annotation')]),
		  label(Label0,
			[optional(true),
			 description('Label of the annotation value')]),
		  comment(Comment,
		      [default(''),
		       description('Optional motivation for or comment about annotation')
		      ])
		]),
	(   setting(login, true)
        ->  ensure_logged_on(User)
        ;   logged_on(User, anonymous)
        ),
	annotation_body(Body0, Body),
	annotation_label(Label0, Body, Label),
	rdf_add_annotation([target(TargetURI),
			    body(Body),
			    field(FieldURI),
			    user(User),
			    label(Label),
			    comment(Comment)
			   ],
			   Annotation,
			   Triples),
	gv_resource_commit(TargetURI,
			   User,
			   add(Triples),
			   Head,
			   Graph),
	reply_json(json([annotation=Annotation,
			 graph=Graph,
			 head=Head])).


%%	http_remove_annotation(+Request)
%
%	Web service to remove resource annotations

http_remove_annotation(Request) :-
	http_parameters(Request,
		[ annotation(Annotation,
		     [uri,
		      description('URI of the annotation object')
		     ])
		]),
	(   setting(login, true)
        ->  ensure_logged_on(User)
        ;   logged_on(User, anonymous)
        ),
	once(rdf(Annotation, oac:hasTarget, Target)),
	findall(rdf(Annotation,O,P), rdf(Annotation,O,P,Graph), Triples),
	gv_resource_commit(Target, User,
			   remove(Triples),
			   Head,
			   Graph),
	reply_json(json([annotation=Annotation,
			 head=Head,
			 graph=Graph])).


%%	http_get_annotation(+Request)
%
%	Web service to get resource annotations

http_get_annotation(Request) :-
	http_parameters(Request,
			[ target(TargetURI,
				 [uri,
				  description('URI of the annotation target')
				 ]),
			  field(FieldURI,
				[uri,
				 optional(true),
				 description('URI of the annotation field')
				])
			]),
	findall(Field,
		(   Field = FieldURI,
		    has_annotation_field(TargetURI, Field)
		),
		Fields0),
	sort(Fields0, Fields),
	findall(Field=json([annotations=JSON]),
		(   member(Field, Fields),
		    json_annotation_list(TargetURI, Field, JSON)
		),
		Annotations),
	reply_json(json(Annotations)).

rdf_add_annotation(Options, Annotation,Triples) :-
	option(comment(Comment), Options),
	(   Comment \= ''
	->  CommentPair = [ po(rdfs:comment,literal(Comment))]
	;   CommentPair = []
	),
	option(user(User), Options),
	option(label(Label), Options),
	option(field(Field), Options),
	option(target(Target), Options),
	option(body(Body), Options),
	KeyValue0 = [
		     po(rdf:type, oac:'Annotation'),
		     po(dcterms:creator,User),
		     po(dcterms:title, literal(Label)),
		     po(an:annotationField, Field),
		     po(oac:hasTarget, Target),
		     po(oac:hasBody, Body)
		     |
		     CommentPair
		    ],
	sort(KeyValue0, KeyValue),
	rdf_global_term(KeyValue, Pairs),
	variant_sha1(Pairs, Hash),
	gv_hash_uri(Hash, Annotation),
	maplist(po2rdf(Annotation),Pairs,Triples).

po2rdf(S,po(P,O),rdf(S,P,O)).


%%	json_annotation_list(+TargetURI, +FieldURI, -Annotations)
%
%	Annotation is a list with annotations represented in prolog JSON
%	notation.

json_annotation_list(Target, FieldURI, JSON) :-
	findall(annotation(A, Body, L, Comment, User),
		annotation_in_field(Target, FieldURI, A, Body, L, Comment, User),
		Annotations),
	prolog_to_json(Annotations, JSON).

has_annotation_field(Target, Field) :-
	gv_resource_head(Target, Commit),
	gv_resource_graph(Commit, Graph),
	rdf(_Annotation, an:annotationField, Field, Graph).

% annotation_in_field/5 is deprecated, use annotation_in_field/7
annotation_in_field(Target, FieldURI, Annotation, Body, Label) :-
	annotation_in_field(Target, FieldURI, Annotation, Body, Label, _Comment, _User).

annotation_in_field(Target, FieldURI, Annotation, Body, Label, Comment, User) :-
	gv_resource_head(Target, Commit),
	gv_resource_graph(Commit, Graph),
	(   setting(user_restrict, true)
	->  logged_on(User, anonymous)
	;   true
	),
	rdf(Annotation, oac:hasTarget, Target, Graph),
	rdf(Annotation, an:annotationField, FieldURI, Graph),
	rdf(Annotation, dcterms:creator, User, Graph),
	rdf(Annotation, oac:hasBody, Body0, Graph),
	rdf(Annotation, dcterms:title, Lit, Graph),
	(   rdf(Annotation, rdfs:comment, Comment0, Graph)
	->  literal_text(Comment0, Comment)
	;   Comment=""
	),
	annotation_body(Body, Body0),
	literal_text(Lit, Label).


annotation_body(literal(L), literal(L)) :- !.
annotation_body(uri(URI), URI).

annotation_label(Label0, Body, Label) :-
	(   var(Label0)
	->  rdf_display_label(Body, Label)
	;   Label = Label0
	).


http:convert_parameter(json_rdf_object, Atom, Term) :-
	atom_json_term(Atom, JSON, []),
	json_to_prolog(JSON, Term).


:- json_object
	annotation(annotation:atom, body:_, label:atom, comment:atom, user:uri),
	uri(value:uri) + [type=uri],
	literal(lang:atom, value:_) + [type=literal],
	literal(type:atom, value:_) + [type=literal],
	literal(value:_) + [type=literal].




