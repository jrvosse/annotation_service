:- module(annotation_api, []).
% No exports: use http api or library(annotation) for access

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(graph_version)).
:- use_module(components(label)).
:- use_module(user(user_db)).
:- use_module(library(oa_schema)).
:- use_module(library(oa_annotation)).


:- setting(login, boolean, true, 'Require login').
:- setting(user_restrict, boolean, false,
	   'When set to true only own annotations are shown.').
:- setting(link_tags, boolean, true,
	   'When set to true tags are hyperlinked to their annotation objects in the UI.').

/***************************************************
* http handlers
***************************************************/

:- http_handler(cliopatria(api/annotation/get),	   http_get_annotation, []).
:- http_handler(cliopatria(api/annotation/add),    http_add_annotation, []).
:- http_handler(cliopatria(api/annotation/remove), http_remove_annotation, []).

:- rdf_meta
	rdf_has_graph(r,r,r,r).

rdf_has_graph(S,P,O,G) :-
	rdf_graph(G),
	rdf_has(S,P,O,RP),
	rdf(S,RP,O,G).

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
		       description('Optional motivation for a comment about the annotation')
		      ])
		]),

	user_url(User),
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
			   Comment,
			   add(Triples),
			   Head,
			   Graph),
	tag_link(annotation, Link),
	reply_json(json([annotation=Annotation,
			 graph=Graph,
			 display_link=Link,
			 head=Head])).


%%	http_remove_annotation(+Request)
%
%	Web service to remove resource annotations

http_remove_annotation(Request) :-
	http_parameters(Request,
		[ annotation(
		      Annotation,
		      [uri,
		       description('URI of the annotation object')
		      ]),
		  comment(
		      Comment,
		      [default(''),
		       description('Optional motivation for a comment about the removal')])
		]),
	user_url(User),
	once(rdf_has(Annotation, oa:hasTarget, Target)),
	findall(rdf(Annotation,O,P), rdf(Annotation,O,P,Graph), Triples),
	gv_resource_commit(Target, User, Comment,
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


%%	json_annotation_list(+TargetURI, +FieldURI, -Annotations)
%
%	Annotation is a list with annotations represented in prolog JSON
%	notation.

json_annotation_list(Target, FieldURI, JSON) :-
	findall(annotation(A, Body, L, D, Comment, User),
		(annotation_in_field(Target, FieldURI, A, Body, L, Comment, User),
		 tag_link(A,D)
		),
		Annotations),
	prolog_to_json(Annotations, JSON).

%%      has_annotation_field(?Target, ?Field) is nondet.
%
%       Evaluates to true if there exists an Annotation
%       on Target for Field in the most recent (eg. the head) Annotation
%       graph for Target.

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
	->  user_url(User)
	;   true
	),
	rdf_has_graph(Annotation, oa:hasTarget, Target, Graph),
	rdf_has_graph(Annotation, an:annotationField, FieldURI, Graph),
	rdf_has_graph(Annotation, oa:annotator, User, Graph),
	rdf_has_graph(Annotation, oa:hasBody, Body0, Graph),
	rdf_has_graph(Annotation, dcterms:title, Lit, Graph),
	(   rdf_has_graph(Annotation, rdfs:comment, Comment0, Graph)
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


user_url(User) :-
	(   setting(login, true)
        ->  ensure_logged_on(U),
	    authorized(write(default, annotate)),
	    user_property(U, url(User))
        ;   logged_on(U)
	->  user_property(U, url(User))
	;   rdf_global_id(an:anonymous, User)
        ).


tag_link(Annotation,Link) :-
	(   setting(link_tags, true)
	->  resource_link(Annotation, Link)
	;   Link = ''
	).

http:convert_parameter(json_rdf_object, Atom, Term) :-
	atom_json_term(Atom, JSON, []),
	json_to_prolog(JSON, Term).


:- json_object
	annotation(
	    annotation:atom,
	    body:_,
	    label:atom,
	    display_link:atom,
	    comment:atom,
	    user:uri),
	uri(value:uri) + [type=uri],
	literal(lang:atom, value:_) + [type=literal],
	literal(type:atom, value:_) + [type=literal],
	literal(value:_) + [type=literal].





