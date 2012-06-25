:- module(oa_annotation, [
			  rdf_add_annotation/3
			 ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(graph_version)).
:- use_module(library(oa_schema)).


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
	time_stamp(T),
	format_time(atom(TimeStamp), '%Y-%m-%dT%H-%M-%S%Oz', T),
	KeyValue0 = [
		     po(rdf:type, oa:'Annotation'),
		     po(oa:annotated, literal(type(xsd:dateTime, TimeStamp))),
		     po(oa:annotator, User),
		     po(dcterms:title, literal(Label)),
		     po(an:annotationField, Field),
		     po(oa:hasTarget, Target),
		     po(oa:hasBody, Body)
		     |
		     CommentPair
		    ],
	sort(KeyValue0, KeyValue),
	rdf_global_term(KeyValue, Pairs),
	variant_sha1(Pairs, Hash),
	gv_hash_uri(Hash, Annotation),
	maplist(po2rdf(Annotation),Pairs,Triples).

po2rdf(S,po(P,O),rdf(S,P,O)).

%%	time_stamp(-Integer)
%
%	Return time-stamp rounded to integer.

time_stamp(Int) :-
	get_time(Now),
	Int is round(Now).
