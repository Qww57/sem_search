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
aff(in, quality, material, beared_by).
aff(in, information, process, affiliation).

aff(by, process, material, agent).
aff(by, process, process, manner).
aff(by, process, artifact, instrument).

aff(of, creation, material, result).
aff(of, transformation, material, patient).
aff(of, quality, material, beared_by).
% aff(of, process, material, beared_by).
aff(of, process, process, has_part).
% aff(of, material, material, part_of).

aff(into, creation, material, direction).
aff(into, transformation, material, result).

aff(from, _, _, _).

aff(at, _, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining basic affinities for compound nouns (production related)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

aff(cn, body_fluid, process, result).
aff(cn, organ, process, agent).
aff(cn, cell, process, agent).







