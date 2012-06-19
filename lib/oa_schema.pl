:- module(oa_schema, []).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_library)).

/** <module> Provide Open Annotation schema, namespace and visualization hooks.

This module provides the OA schema and the  prefix =oa= and =oax= for use in
Prolog.
*/

% Namespace for package-specific extensions:
:- rdf_register_ns(an,  'http://semanticweb.cs.vu.nl/annotate/').

% Namespace for old open annotation model:
:- rdf_register_ns(oac, 'http://www.openannotation.org/ns/').

% Namespaces for new open annotation model. Warning: under development!
:- rdf_register_ns(oa,  'http://www.w3.org/ns/openannotation/core/').
:- rdf_register_ns(oax,	'http://www.w3.org/ns/openannotation/extension/').

:- rdf_attach_library(annotation_service(rdf)).
% :- rdf_load_library(oa).

