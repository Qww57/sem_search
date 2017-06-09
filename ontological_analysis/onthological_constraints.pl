%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Onthological_constraints.pl:
%
%               Set of rules and affinities for onthological
%                disambiguation of Natural Logics Grammar.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [onthology_skeleton].   % Loading the is_subset_of predicate.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining transitive rules for constraints.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lex(X, trans, const(Y,M))  :-  isa(Y,Z), lex(X, trans, const(Z,M)), !.
lex(X, trans, const(Y,M))  :-  isa(M,N), lex(X, trans, const(Y,N)), !. % FIXME
lex(X, adj, const(Y), M)   :-  isa(Y,Z), lex(X, adj, const(Z), M), !.

isa(X,Y)                   :-  isa(X,Y,[]).
isa(X,Y,V)		   :-  is_subset_of(X,Y), \+ memberchk((X,Y),V).
isa(X,Y,V)		   :-  is_subset_of(X,Z), \+ memberchk((X,Z),V),
			       isa(Z,Y,[(X,Y)|V]).

aff(P,Y,S2,R)  :-  isa(Y,S1), aff(P,S1,S2,R).
aff(P,S1,Y,R)  :-  isa(Y,S2), aff(P,S1,S2,R).


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







