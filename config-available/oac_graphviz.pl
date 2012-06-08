:- module(oac_graphviz, []).


:- use_module(cliopatria(hooks)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_abstract)).

:- rdf_meta
        context_triple(r, t).

cliopatria:context_graph(URI, RDF) :-
	rdf(_, gv:graph, URI),
	!,
	findall(T, graph_context_triple(URI, T), RDF0),
	sort(RDF0, RDF1),
	minimise_graph(RDF1, RDF2),		% remove inverse/symmetric/...
	bagify_graph(RDF2, RDF3, Bags, []),	% Create bags of similar resources
	append(RDF3, Bags, RDF),
	RDF \= [].

graph_context_triple(G, rdf(S,P,O)) :-
	rdf_equal(P, oac:hasTarget),
	rdf(S, P, O, G).
graph_context_triple(G, rdf(S,P,G)) :-
	rdf_equal(P, gv:graph),
	rdf(S,P,G).
graph_context_triple(G, rdf(H,P,T)) :-
	rdf(_, oac:hasTarget, T,G),
	rdf_equal(P, gv:head),
	rdf(H,P,T).

cliopatria:node_shape(URI, Shape, _Options) :-
	rdfs_individual_of(URI, oac:'Annotation'),
	Shape = [shape('Mdiamond'),fontize('20.00'), style(filled),fillcolor('#FF8888')].
