:- module(annotation_api, []).
% No exports: use http api or library(oa_annotation) for access

% HTTP:
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).

% SemWeb:
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).

% ClioPatria:
:- use_module(components(label)).
:- use_module(user(user_db)).

% Other cpacks:
:- use_module(library(graph_version)).

% This cpack:
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
		  typing_time(TypingTime,
			      [default(0),
			       description("Time it took to type in milliseconds")
			      ]),
		  unsure(Unsure,
			      [default(false),
			       boolean,
			       description("Set to true when unsure of validity of annotation")
			      ]),
		  comment(UserComment,
		      [default(''),
		       description('Optional motivation for a comment about the annotation')
		      ])
		]),

	user_url(User),
	annotation_body(Body0, Body),
	annotation_label(Label0, Body, Label),
	format(atom(CommitComment), 'add annotation: ~w on ~w~n~n~w', [Body, TargetURI, UserComment]),
	with_mutex(TargetURI,
		   (   rdf_add_annotation(
			   [target(TargetURI),
			    body(Body),
			    field(FieldURI),
			    user(User),
			    label(Label),
			    graph(TargetURI),
			    comment(UserComment),
			    unsure(Unsure),
			    typing_time(TypingTime)
			   ],
			   Annotation),
		       gv_resource_commit(
			   TargetURI,
			   User,
			   CommitComment,
			   Head))),

	tag_link(Annotation, Link),
	reply_json(json([annotation=Annotation,
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
		      UserComment,
		      [default(''),
		       description('Optional motivation for a comment about the removal')])
		]),
	user_url(User),

	rdf(Annotation, oa:hasBody, Body),
	rdf(Annotation, oa:hasTarget, TargetURI),
	!,
	format(atom(CommitComment), 'rm annotation: ~w on ~w~n~n~w', [Body, TargetURI, UserComment]),
	with_mutex(TargetURI,
		   (   rdf_remove_annotation(Annotation, TargetURI),
		       gv_resource_commit(TargetURI, User, CommitComment, Head))),
	reply_json(json([annotation=Annotation,
			 head=Head])).

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
	Options = [annotationField(FieldURI), user(User) | Options1 ],
	(   setting(user_restrict, true)
	->  user_url(User)
	;   true
	),
	findall(Annotation,
		(   rdf_get_annotation(TargetURI, TargetURI, Options),
		    option(annotation(A), Options),
		    tag_link(A,Link),
		    select_option(body(Body), Options1, Options2),
		    prolog_to_json(Body, BodyJson),
		    Annotation = json([body(BodyJson),
				       user(User),
				       display_link(Link) |
				       Options2])
		),
		Annotations),
	JSON =.. [ FieldURI, json([annotations(Annotations)])],
	reply_json(json([JSON])).



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
            unsure:atom,
            user:uri),
        uri(value:uri) + [type=uri],
        literal(lang:atom, value:_) + [type=literal],
        literal(type:atom, value:_) + [type=literal],
        literal(value:_) + [type=literal].
