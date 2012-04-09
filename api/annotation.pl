:- module(annotation_api, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(graph_version)).
:- use_module(user(user_db)).

:- rdf_register_ns(oac, 'http://www.openannotation.org/ns/').
:- rdf_register_ns(an, 'http://semanticweb.cs.vu.nl/annotate/').

:- setting(login, boolean, true, 'Require login').

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
			 description('Label of the annotation value')])
		]),
	(   setting(login, true)
        ->  ensure_logged_on(User)
        ;   logged_on(User, anonymous)
        ),
	annotation_body(Body0, Body),
	annotation_label(Label0, Body, Label),
	gv_resource_commit(TargetURI, User,
			   rdf_add_annotation(Graph, User, TargetURI, FieldURI, Body, Label, Annotation),
			   Head,
			   Graph),
	reply_json(json([annotation=Annotation,
			 graph=Graph,
			 head=Head])).

rdf_add_annotation(Graph, User, Target, Field, Body, Label, Annotation) :-
	rdf_bnode(Annotation),
	rdf_assert(Annotation, dcterms:creator, User, Graph),
	rdf_assert(Annotation, an:annotationField, Field, Graph),
	rdf_assert(Annotation, rdf:type, oac:'Annotation', Graph),
	rdf_assert(Annotation, oac:hasTarget, Target, Graph),
	rdf_assert(Annotation, oac:hasBody, Body, Graph),
	rdf_assert(Annotation, dcterms:title, literal(Label), Graph).



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
			 description('Label of the annotation value')])
		]),
	(   setting(login, true)
        ->  ensure_logged_on(User)
        ;   logged_on(User, anonymous)
        ),
	annotation_body(Body0, Body),
	annotation_label(Label0, Body, Label),
	gv_resource_commit(TargetURI, User,
			   rdf_update_annotation(Graph, Annotation, User, TargetURI, FieldURI, Body, Label),
			   Head,
			   Graph),
	reply_json(json([annotation=Annotation,
			 graph=Graph,
			 head=Head])).

rdf_update_annotation(Graph, Annotation, User, Target, Field, Body, Label) :-
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
	rdf_assert(Annotation, oac:hasBody, Body, Graph),
	rdf_assert(Annotation, dcterms:title, literal(Label), Graph).


		 /*******************************
		 *               Utils		*
		 *******************************/

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
	annotation(annotation:atom, body:_, label:atom),
	uri(value:uri) + [type=uri],
	literal(lang:atom, value:_) + [type=literal],
	literal(type:atom, value:_) + [type=literal],
	literal(value:_) + [type=literal].
