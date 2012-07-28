:- module(oa_annotation, [
			  rdf_add_annotation/2,
			  rdf_remove_annotation/2
			 ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(graph_version)).
:- use_module(library(oa_schema)).


rdf_add_annotation(Options, Annotation) :-
	option(comment(Comment), Options),
	(   Comment \= ''
	->  CommentPair = [ po(rdfs:comment,literal(Comment))]
	;   CommentPair = []
	),
	option(unsure(Unsure),	Options, false),
	(   Unsure == true
	->  UnsurePair = [ po(an:unsure, literal(Unsure)) ]
	;   UnsurePair = []
	),

	option(user(User),     Options),
	option(label(Label),   Options),
	option(field(Field),   Options),
	option(target(Target), Options),
	option(body(Body),     Options),
	option(typing_time(TT),	Options, 0),
	option(graph(Graph),   Options, 'annotations'),
	get_time(T),
	format_time(atom(TimeStamp), '%FT%T%:z', T), % xsd:dateTime
	KeyValue0 = [
		     po(rdf:type, oa:'Annotation'),
		     po(oa:annotated, literal(type(xsd:dateTime, TimeStamp))),
		     po(oa:annotator, User),
		     po(oa:hasTarget, Target),
		     po(oa:hasBody, Body),
		     po(dcterms:title, literal(Label)),
		     po(an:annotationField, Field),
		     po(an:typingTime, literal(type(xsd:integer, TT)))
		    ],
	append([KeyValue0, CommentPair, UnsurePair], KeyValue1),
	sort(KeyValue1, KeyValue),
	rdf_global_term(KeyValue, Pairs),
	variant_sha1(Pairs, Hash),
	gv_hash_uri(Hash, Annotation),
	maplist(po2rdf(Annotation),Pairs,Triples),
	gv_graph_triples(Graph, Triples).

po2rdf(S,po(P,O),rdf(S,P,O)).

%%	rdf_get_annotation(+Annotation, +Graph, -PropList) is det.
%
%	To be implemented later.

rdf_get_annotation(+Annotation, +Graph, Props) :-
	rdf(Annotation, oa:hasTarget, Target, Graph),
	Props =
	[ target(Target)
	].

%%	rdf_remove_annotation(+Annotation, ?Target) is det.
%
%	Removes Annotation on Target. Succeeds also if Annotation
%	does not exists.

rdf_remove_annotation(Annotation, Target) :-
	(   rdf(Annotation, oa:hasTarget, Target, Target)
	->  rdf_retractall(Annotation, _, _, Target)
	;   true
	).
