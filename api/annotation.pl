:- module(annotation_api, []).
% No exports: use http api or library(oa_annotation) for access

% HTTP:
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
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
		  hasBody(BodyObject,
		       [optional(false),
			description('Body of the annotation')]),
		  graph(Graph,
			[ optional(true),
			  description('Named graph to store the triples of this annotation in.  Defaults to uri of the target')
			  ]),
		  field(FieldURI,
			[uri,
			 description('URI of the annotation field')
			]),
		  label(Label,
			[optional(true),
			 description('Label of the annotation value')]),
		  typing_time(TypingTime,
			      [default(0),
			       description("Time it took to type in milliseconds")
			      ]),
		  motivatedBy(Motivation,
			      [ uri,
				default(oa:tagging),
				description('motivation')
			      ]),
		  type(Type,
		       [ uri,
			 optional(true),
			 description("Annotation type")
		       ])
		]),
	user_url(User),
	atom_json_dict(TargetObject,TargetDictList,[]),
	member(TargetDict, TargetDictList),
	( atom_string(TargetURI, TargetDict.get('@id')) ;
	  atom_string(TargetURI, TargetDict.hasSource)
	),

	( var(Graph)
	  -> Graph = TargetURI
	  ; true
	),

	annotation_body(BodyObject, BodyDict, Label),
	format(atom(CommitComment), 'add annotation: ~w on ~w~n~n', [Label, TargetURI]),
	with_mutex(TargetURI,
		   (   rdf_add_annotation(
			   [target(TargetDictList),
			    body(BodyDict),
			    field(FieldURI),
			    user(User),
			    label(Label),
			    graph(Graph),
			    type(Type),
			    motivatedBy(Motivation),
			    typing_time(TypingTime)
			   ],
			   Annotation),
		       commit_when_needed(
			   Graph,
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

	rdf_has(Annotation, oa:hasBody, Body),
	rdf_get_annotation_target(Annotation, TargetURI),
	Graph = TargetURI,
	!,
	format(atom(CommitComment), 'rm annotation: ~w on ~w~n~n~w', [Body, TargetURI, UserComment]),
	with_mutex(TargetURI,
		   (   rdf_remove_annotation(Annotation),
		       remove_meta_annotations(Annotation),
		       commit_when_needed(Graph, User, CommitComment, Head))),
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
	(   setting(user_restrict, true),
	    \+ logged_on(admin)
	->  user_url(FilterUser)
	;   true
	),

	findall(A,
		(   rdf_get_annotation_by_tfa(TargetURI, FieldURI, FilterUser, _Graph, Options),
		    option(annotation(A), Options)
		),
		Annotations).

enrich_annotation(A, Json) :-
	tag_link(A,Link),
	rdf_get_annotation(A, AnOptions),
	screen_name(AnOptions, ScreenName),
	Json = json(
		   ['@context'('http://www.w3.org/ns/oa.json'),
		    '@id'(A),
		    annotation(A),
		    screenName(ScreenName),
		    display_link(Link) |
		    AnOptions]).

annotation_body(JSON, Dict, Label) :-
	atom_json_dict(JSON, Dict, []),
	(   Dict.get('@id') = URI
	->  annotation_label(URI, Label)
	;   atom_string(Label,Dict.get('@value'))
	).

annotation_label(Body, Label) :-
	(   var(Label)
	->  rdf_display_label(Body, Label)
	;   true
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
	option(annotatedBy(Annotator), Annotation, Anonymous),
	iri_xml_namespace(Annotator, _, ScreenName).

remove_meta_annotations(A) :-
	findall(M, rdf_has(M, oa:hasTarget, A), Metas),
	forall(member(M, Metas),
	       rdf_remove_annotation(M)
	      ).
