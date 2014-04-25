:- module(annotation_api, []).
% No exports: use http api or library(oa_annotation) for access

% HTTP:
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
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
:- setting(track, boolean, true, 'Version track annotations using graph_version cpack').
:- setting(user_restrict, boolean, false,
	   'When set to true only user\'s own annotations are shown.').
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
		[ hasTarget(TargetObject,
		     [optional(false),
		      description('URI (or complex target object) of the resource that is annotated')
		     ]),
		  field(FieldURI,
			[uri,
			 description('URI of the annotation field')
			]),
		  hasBody(Body0,
		       [json_rdf_object,
			description('Body of the annotation')]),
		  label(Label0,
			[optional(true),
			 description('Label of the annotation value')]),
		  typing_time(TypingTime,
			      [default(0),
			       description("Time it took to type in milliseconds")
			      ]),
		  type(Type,
			      [default(tag),
			       atom,
			       description("Annotation type")
			      ])
		]),
	user_url(User),
	(   atom_json_dict(TargetObject,TargetDict,[])
	    -> atom_string(TargetURI,TargetDict.hasSource)
	    ;  (TargetDict = target{'@id':TargetObject} ,
	        TargetURI  = TargetObject
	       )
	),


	annotation_body(Body0, Body),
	annotation_label(Label0, Body, Label),
	format(atom(CommitComment), 'add annotation: ~w on ~w~n~n', [Body, TargetURI]),
	with_mutex(TargetURI,
		   (   rdf_add_annotation(
			   [target(TargetDict),
			    body(Body),
			    field(FieldURI),
			    user(User),
			    label(Label),
			    graph(TargetURI),
			    type(Type),
			    typing_time(TypingTime)
			   ],
			   Annotation),
		       commit_when_needed(
			   TargetURI,
			   User,
			   CommitComment,
			   _Head))),

	enrich_annotation(Annotation, Json),
	reply_json(Json).

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
	rdf_get_annotation_target(Annotation, TargetURI),
	!,
	format(atom(CommitComment), 'rm annotation: ~w on ~w~n~n~w', [Body, TargetURI, UserComment]),
	with_mutex(TargetURI,
		   (   rdf_remove_annotation(Annotation, TargetURI),
		       commit_when_needed(TargetURI, User, CommitComment, Head))),
	reply_json(json([annotation=Annotation,
			 head=Head])).

http_get_annotation(Request) :-
	http_parameters(Request,
			[ hasTarget(TargetURI,
				 [uri,
				  description('URI of the annotation target')
				 ]),
			  field(FieldURI,
				[uri,
				 optional(true),
				 description('URI of the annotation field')
				])
			]),
	collect_annotations([TargetURI], FieldURI, Annotations),
	maplist(enrich_annotation, Annotations, JsonAnnotations),
	JSON =.. [ FieldURI, json([annotations(JsonAnnotations)])],
	reply_json(json([JSON])).

commit_when_needed(NamedGraph, User, Comment, Head) :-
	(   setting(track, true)
	->  gv_resource_commit(NamedGraph, User, Comment, Head)
	;   true
	).

collect_annotations([], _, []) :- !.
collect_annotations([TargetURI|Tail], FieldURI, AllAnnotations) :-
	collect_target_annotation(TargetURI, FieldURI, TargetAnnotations),
	collect_annotations(TargetAnnotations, FieldURI, MetaAnnotations),
	collect_annotations(Tail, FieldURI, TailAnnotations),
	append([TargetAnnotations, MetaAnnotations, TailAnnotations], AllAnnotations).

collect_target_annotation(TargetURI, FieldURI, Annotations) :-
	(   setting(user_restrict, true)
	->  user_url(User)
	;   true
	),

	findall(A,
		(   rdf_get_annotation_by_tfa(TargetURI, FieldURI, User, _Graph, Options),
		    option(annotation(A), Options)
		),
		Annotations).

enrich_annotation(A, Json) :-
	tag_link(A,Link),
	rdf_get_annotation(A, AnOptions),
	screen_name(AnOptions, ScreenName),
	select_option(hasBody(Body), AnOptions, AnOptions1),
	prolog_to_json(Body, JsonBody),

	Json = json([annotation(A),
		     hasBody(JsonBody),
		     screenName(ScreenName),
		     display_link(Link) |
		     AnOptions1]).


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
	;   rdf_global_id(user:anonymous, User)
        ).


tag_link(Annotation,Link) :-
	(   setting(link_tags, true)
	->  resource_link(Annotation, Link)
	;   Link = ''
	).

screen_name(Annotation, ScreenName) :-
	rdf_equal(user:anonymous, Anonymous),
	option(annotator(Annotator), Annotation, Anonymous),
	iri_xml_namespace(Annotator, _, ScreenName).

http:convert_parameter(json_rdf_object, Atom, Term) :-
        atom_json_term(Atom, JSON, []),
        json_to_prolog(JSON, Term).


:- json_object
        annotation(
            annotation:atom,
            hasBody:_,
            label:atom,
            display_link:atom,
            type:atom,
            user:uri),
        uri(value:uri) + [type=uri],
        literal(lang:atom, value:_) + [type=literal],
        literal(type:atom, value:_) + [type=literal],
        literal(value:_) + [type=literal].
