:- module(annotation_api, [
			   rdf_add_annotation/8,
			   rdf_update_annotation/7,
			   annotation_in_field/5,
			   json_annotation_list/3
			  ]).

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
:- http_handler(cliopatria(api/annotation/update), http_update_annotation, []).
:- http_handler(cliopatria(api/annotation/remove), http_remove_annotation, []).

%%	http_add_annotation(+Request)
%
%	Web service to add resource annotations
%
%	%hack
%	in the add service we store the user explicitly in the
%	annotation. in the remove and update services we do not look at
%	this, in the update this can give inconsistencies.

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
	gv_resource_commit(TargetURI, User,
			   rdf_add_annotation(Graph, User, TargetURI, FieldURI, Body, Label, Comment, Annotation),
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
	gv_resource_commit(Target, User,
			   rdf_retractall(Annotation, _, _, Graph),
			   Head,
			   Graph),
	reply_json(json([annotation=Annotation,
			 head=Head,
			 graph=Graph])).

%%	http_update_annotation(+Request)
%
%	Web service to update resource annotations

http_update_annotation(Request) :-
	http_parameters(Request,
		[ target(TargetURI,
		     [uri,
		      description('URI of the object that is annotated')
		     ]),
		  field(FieldURI,
			[uri,
			 description('URI of the annotation field')
			]),
		  annotation(Annotation,
			     [optional(true),
			      description('Annotation for which the body is updated')]),
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
	gv_resource_commit(TargetURI, User,
			   rdf_update_annotation(Graph, Annotation, User, TargetURI, FieldURI, Body, Label, Comment),
			   Head,
			   Graph),
	reply_json(json([annotation=Annotation,
			 graph=Graph,
			 head=Head])).

rdf_add_annotation(Graph, User, Target, Field, Body, Label, Comment, Annotation) :-
	rdf_bnode(Annotation),
	rdf_assert(Annotation, rdf:type, oac:'Annotation', Graph),
	rdf_assert(Annotation, dcterms:creator, User, Graph),
	rdf_assert(Annotation, dcterms:title, literal(Label), Graph),
	rdf_assert(Annotation, an:annotationField, Field, Graph),
	rdf_assert(Annotation, oac:hasTarget, Target, Graph),
	rdf_assert(Annotation, oac:hasBody, Body, Graph),
	(   Comment \= ''
	->  rdf_assert(Annotation, dcterms:comment, literal(Comment), Graph)
	;   true
	).


rdf_update_annotation(Graph, Annotation, User, Target, Field, Body, Label) :-
	rdf_update_annotation(Graph, Annotation, User, Target, Field, Body, Label, '').

rdf_update_annotation(Graph, Annotation, User, Target, Field, Body, Label, Comment) :-
	(   var(Annotation)
	->  rdf_bnode(Annotation),
	    rdf_assert(Annotation, an:annotationField, Field, Graph),
	    rdf_assert(Annotation, rdf:type, oac:'Annotation', Graph),
	    rdf_assert(Annotation, oac:hasTarget, Target, Graph)
	;   rdf_retractall(Annotation, oac:hasBody, _, Graph),
	    rdf_retractall(Annotation, dcterms:title, _, Graph),
	    rdf_retractall(Annotation, dcterms:creator, _, Graph)
	),
	rdf_assert(Annotation, dcterms:creator, User, Graph),
	rdf_assert(Annotation, dcterms:title, literal(Label), Graph),
	rdf_assert(Annotation, oac:hasBody, Body, Graph),
	(   Comment \= ''
	->  rdf_assert(Annotation, dcterms:comment, literal(Comment), Graph)
	;   true
	).


%%	json_annotation_list(+TargetURI, +FieldURI, -Annotations)
%
%	Annotation is a list with annotations represented in prolog JSON
%	notation.

json_annotation_list(Target, FieldURI, JSON) :-
	findall(annotation(A, Body, L, Comment),
		annotation_in_field(Target, FieldURI, A, Body, L, Comment),
		Annotations),
	prolog_to_json(Annotations, JSON).

% annotation_in_field/5 is deprecated, use annotation_in_field/6
annotation_in_field(Target, FieldURI, Annotation, Body, Label) :-
	annotation_in_field(Target, FieldURI, Annotation, Body, Label, _Comment).

annotation_in_field(Target, FieldURI, Annotation, Body, Label, Comment) :-
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
	(   rdf(Annotation, dcterms:comment, Comment0, Graph)
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
	annotation(annotation:atom, body:_, label:atom, comment:atom),
	uri(value:uri) + [type=uri],
	literal(lang:atom, value:_) + [type=literal],
	literal(type:atom, value:_) + [type=literal],
	literal(value:_) + [type=literal].




