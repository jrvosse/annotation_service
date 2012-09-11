:- module(oa_annotation, [
			  rdf_has_graph/4,
			  rdf_add_annotation/2,
			  rdf_get_annotation/3,
			  rdf_remove_annotation/2
			 ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(graph_version)).
:- use_module(library(oa_schema)).

:- rdf_meta
	rdf_has_graph(r,r,r,r).

rdf_has_graph(S,P,O,G) :-
	rdf_graph(G),
	rdf_has(S,P,O,RP),
	rdf(S,RP,O,G).

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

%%	rdf_get_annotation(+Annotation, +Graph, -Props) is nondet.
%
%	Props is an option list with the properties of Annotation
%	in Graph.
%
%	Hack:- You can filter on annotationField(F), user(U) by putting 
%	these in the Props as the first two properties...
rdf_get_annotation(Target, Graph, Props) :-
	rdf(Annotation, oa:hasTarget, Target, Graph),
	rdf(Annotation, an:annotationField, Field, Graph),
	rdf_has_graph(Annotation, oa:annotator, User, Graph),
	rdf_has_graph(Annotation, oa:hasBody, Body, Graph),
	rdf_has_graph(Annotation, dcterms:title, Lit, Graph),
	(   rdf_has_graph(Annotation, rdfs:comment, Comment0, Graph)
	->  literal_text(Comment0, Comment)
	;   Comment=''
	),
	(   rdf_has_graph(Annotation, an:unsure, Unsure0, Graph)
	->  literal_text(Unsure0, Unsure)
	;   Unsure=''
	),
	literal_text(Lit, Label),

	Props = [
		 annotationField(Field),
		 user(User),
		 annotation(Annotation),
		 comment(Comment),
		 unsure(Unsure),
		 label(Label),
		 body(Body)
	].

%%	rdf_remove_annotation(+Annotation, ?Target) is det.
%
%	Removes Annotation on Target. Also succeeds if Annotation
%	does not exists.

rdf_remove_annotation(Annotation, Target) :-
	(   rdf(Annotation, oa:hasTarget, Target, Target)
	->  rdf_retractall(Annotation, _, _, Target)
	;   true
	).
