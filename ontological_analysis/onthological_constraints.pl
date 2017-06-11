%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Onthological_constraints.pl:
%
%               Set of rules and affinities for onthological
%                disambiguation of Natural Logics Grammar.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded([onthology_skeleton]).	% Loading the is_subset_of predicate.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining basic affinities for prepositional phrases
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aff(in, material, material, location).
aff(in, process, material, location).
aff(in, quality, material, location).
aff(in, information, process, affiliation).
aff(in, process, quality, manner).

aff(by, process, body_part, agent).
aff(by, process, process, manner).
aff(by, process, artifact, instrument).
aff(by, process, body_fluid, agent).
aff(by, enhancement, quality, agent). % for nomi 240

aff(of, creation, material, result).
aff(of, transformation, material, patient).
aff(of, enhancement, process, patient).
aff(of, process, body_part, beared_by).
aff(of, process, material, patient).
aff(of, process, quality, patient). % for nomi 230
aff(of, process, information, patient). % for nomi
aff(of, quality, material, beared_by).
aff(of, process, process, has_part).
aff(of, body_part, material, part_of).

aff(into, transformation, material, result).
aff(into, creation, material, direction).
aff(into, process, material, direction).

aff(from, process, body_fluid, source).
aff(from, body_fluid, body_fluid, source).
aff(from, body_fluid, body_part, source).
aff(from, process, body_part, source).

aff(at, entity, quality, condition).

aff(as, quality, entity, comparison).

aff(in_response_to, process, quality, cause).
aff(in_response_to, process, process, cause).

aff(within, entity, entity, location).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining basic affinities for compound nouns (production related)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aff(cn, body_fluid, process, result).
aff(cn, organ, process, agent).
aff(cn, cell, process, agent).
aff(cn, process, process, manner).
aff(cn, material, quality, bearer).

aff(cn, body_part, process, bearer).
aff(cn, body_part, body_part, characterization).
aff(cn, protein, protein, characterization).
aff(cn, material, material, characterization).








