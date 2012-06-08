:- module(conf_annotation_service, []).

/** <module> Configurable RDF annotation webservice
*/

:- use_module(library(semweb/rdf_db)).
:- rdf_register_ns(oac, 'http://www.openannotation.org/ns/').
:- rdf_register_ns(an, 'http://semanticweb.cs.vu.nl/annotate/').

