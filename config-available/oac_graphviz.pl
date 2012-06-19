:- module(oa_graphviz, []).

:- use_module(cliopatria(hooks)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_abstract)).

% Need these modules solely for their namespace declarations:
:- use_module(library(graph_version)).
:- use_module(library(oa_schema)).

:- rdf_meta
        graph_context_triple(r, t).

cliopatria:context_graph(URI, RDF) :-
	rdf(_, gv:graph, URI),			% assume URI is a graph with annotations
	!,
	findall(T, graph_context_triple(URI, T), RDF0),
	sort(RDF0, RDF1),
	minimise_graph(RDF1, RDF2),		% remove inverse/symmetric/...
	bagify_graph(RDF2, RDF3, Bags, []),	% Create bags of similar resources
	append(RDF3, Bags, RDF),
	RDF \= [].

cliopatria:context_graph(URI, RDF) :-
	rdfs_individual_of(URI, oa:'Annotation'),% assume URI is an Annotations
	!,
	findall(T, annotation_context_triple(URI, T), RDF0),
	sort(RDF0, RDF1),
	minimise_graph(RDF1, RDF2),		% remove inverse/symmetric/...
	bagify_graph(RDF2, RDF3, Bags, []),	% Create bags of similar resources
	append(RDF3, Bags, RDF),
	RDF \= [].

graph_context_triple(G, rdf(S,P,O)) :-
	rdf_equal(P, oa:hasTarget),
	rdf(S, P, O, G).
graph_context_triple(G, rdf(S,P,O)) :-
	rdf_equal(P, oac:hasTarget),
	rdf(S, P, O, G).
graph_context_triple(G, rdf(S,P,G)) :-
	rdf_equal(P, gv:graph),
	rdf(S,P,G).
graph_context_triple(G, rdf(H,P,T)) :-
	(   rdf(_, oa:hasTarget, T, G) ; rdf(_, oac:hasTarget, T, G)),
	rdf_equal(P, gv:head),
	rdf(H,P,T).

annotation_context_triple(S, rdf(S,P,O)) :-
	rdf_equal(oa:hasTarget, P),
	rdf_has(S,P,O).
annotation_context_triple(S, rdf(S,P,O)) :-
	rdf_equal(oa:hasBody, P),
	rdf_has(S,P,O).
annotation_context_triple(S, rdf(S,P,O)) :-
	rdf_equal(oa:annotator, P),
	rdf_has(S,P,O).

annotation_context_triple(S, rdf(Commit,P,G)) :-
	rdf(S,rdf:type,_,G),
	rdf_equal(gv:graph, P),
	rdf(Commit, P, G).

cliopatria:node_shape(URI, Shape, _Options) :-
	rdfs_individual_of(URI, oa:'Annotation'),
	Shape = [shape('Mdiamond'),fontize('20.00'), style(filled),fillcolor('#FF8888')].


cliopatria:node_shape(URI, Shape, _Options) :-
	rdf(URI, gv:head, _),
	Shape = [fontize('20.00'), style(filled),fillcolor('#FF8888')].



