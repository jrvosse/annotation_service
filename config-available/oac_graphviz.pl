:- module(oa_graphviz, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).

:- use_module(cliopatria(hooks)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_abstract)).

:- use_module(applications(browse)).

:- use_module(library(oa_annotation)).

% Need these modules solely for their namespace declarations:
:- use_module(library(oa_schema)).
:- use_module(library(graph_version)).

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
annotation_context_triple(W, rdf(S,P2,T)) :-
	rdf_equal(P1, oa:hasTarget),
	rdf_equal(P2, oa:hasSource),
	rdf_has(W, P1, S),
	rdf_has(S, P2, T).
annotation_context_triple(S, rdf(S,P,O)) :-
	rdf_equal(oa:hasBody, P),
	rdf_has(S,P,O).
annotation_context_triple(S, rdf(S,R,O)) :-
	rdf_equal(oa:annotatedBy, P),
	rdf_has(S,P,O,R).

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


%%	list_resource(URI, Options)// is det.
%
%	Extends basic list_resource/2 functionality for resources that
%	are an annotation target.

cliopatria:list_resource(URI, Options) -->
	{
	       rdf_get_annotation_target(_, URI),
	       http_link_to_id(http_annotation, [target(URI)], AnnotateLink)
	},
	html([]),
	html([
	    div(['Existing annotations: ',
	    % \(cpa_browse:as_object(URI, URI)) FIX me
		]),
	    a(href(AnnotateLink), 'Make new annotations')
	]),
	list_resource(URI, [raw(true) | Options]).
