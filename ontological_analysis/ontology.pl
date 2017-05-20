%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Extensions of Natural Logics:
% (on noun phrases)
% - fully recursive structures: OK
% - relative clauses: OK (post)
% - prepositional phrases: OK (post)
% - compound nouns: OK (pre)
% - possessives: OK (pre)
% - adjectives: OK (pre)
% - appositions: TODO (post)
% (on verbs)
% - prepositional verbs: TODO
% - passive forms: TODO
% - passivisation: TODO
% - nominalisation: TODO
% - adverbs: TODO
% - adverbial prepositional phrase: TODO
% - expressing conditions: TODO
% (on propositions)
% - conjonctions: TODO
% - disjunctions: TODO
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Constrainted Grammar.
proposition(s(NP,VP))           -->   np(NP, const(S)), vp(VP, const(S)).
vp(vp(V,N), const(S))           -->   rterm(V, const(S, C)), np(N, const(C)).
vp(vp(V), const(S))             -->   rterm(V, const(S)).

np(np1(N), const(S))		-->   n(N, const(S)).
% np(np2(N, mod(M)), const(S))	-->   n(N, const(S)), post(M, const(S)).
% np(np3(N, mod(M)), const(S))  -->   pre(M, const(S)), n(N, const(S)).
np(np4(N, mod(M)), const(S))	-->   pre(Q, const(S)), n(N,const(S)),
				      post(P, const(S)), {append(Q,P,M)},
				      {length(M,L)}, {L > 0}.

% np(np5(N, mod(M)), const(S))	-->   cn(N1, _), ['s'],
%				      n(N, const(S)), post(P, const(S)),
%				      {M = [ger([affiliation], N1)|P]}.
% np(np6(N, mod(M)), const(S))	-->   cn(N1, _), ['s'],
%				      pre(Q, const(S)), n(N, const(S)),
%				      {M = [ger([affiliation], N1)|Q]}.
np(np7(N, mod(M)), const(S))	-->   cn(N1, _), ['s'],
				      pre(Q, const(S)), n(N, const(S)), post(P, const(S)),
				      {append([ger([affiliation], N1)|Q],P,M)}.

% To allow compound nouns in possessives.
cn(np(N), const(S))             -->   n(N, const(S)).
cn(np(N, mod(M)), const(S))     -->   pre(M, const(S)), n(N, const(S)).

% Defining pre-modifiers
pre([adj([R],A)], const(S))     -->   adj(A, const(S), R).
pre([cn([R],N1)], const(S2))	-->   n(N1, const(S1)), {aff(cn, S1, S2, R)}.
pre([adj([R],A)|T], const(S))	-->   adj(A, const(S), R), pre(T, const(S)).
pre([cn([R], N1)|T], const(S2))	-->   n(N1, const(S1)), {aff(cn, S1, S2, R)},
	                              pre(T, const(S2)).
pre([], const(_))               -->   {true}.

% Defining post-modifiers
post([pp(P,[R],N)], const(S1))   -->   prep(P, const(S1, S2), R), np(N, const(S2)).
post([rc(V,N)], const(S1))       -->   [that], rterm(V, const(S1,S2)), np(N, const(S2)).
post([pp(P,[R],N)|T], const(S1)) -->   prep(P, const(S1, S2), R), np(N, const(S2)),
				       align(_), post(T, const(S1)).
post([pp(P,[R],N)|T], const(S1)) -->   prep(P, const(S1, S2), R), np(N, const(S2)),
				       post(T, const(S1)).
post([rc(V,N)|T], const(S1))     -->   [that], rterm(V, const(S1,S2)), np(N,const(S2)),
				       align(_), post(T, const(S1)).
post([rc(V,N)|T], const(S1))	 -->   [that], rterm(V, const(S1,S2)), np(N, const(S2)),
	                               post(T, const(S1)).
post([], const(_))	         -->   {true}.

prep(P, const(S1, S2), R)       -->   [P], {lex(P, preposition)}, {aff(P, S1, S2, R)}.
n(n(N), const(S))	        -->   [N], {lex(N, noun, S)}.
adj(A, const(S), R)		-->   [A], {lex(A, adj, const(S), R)}.

rterm(verb(isa), _)		-->   [isa].
rterm(verb(V), const(S,C))      -->   [V], {lex(V, trans, const(S,C))}.
rterm(verb(V), const(S))        -->   [V], {lex(V, intrans, const(S))}.

align(X)                        -->   [X], {X = and}.
align(X)                        -->   [X], {X = ','}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- discontiguous lex/3.
:- discontiguous lex/4.


% Lexicon: prepositions
% lex(word:preposition, type:preposition)
lex(in, preposition).
lex(of, preposition).
lex(by, preposition).

% Lexicon: nouns
% lex(word:noun, type:noun, ontology:constraints).
lex(doctor, noun, human).
lex(table, noun, furniture).
lex(disease, noun, process).
lex(production, noun, creation).
lex(glucogenesis, noun, creation).
lex(conversion, noun, transformation).
lex(glucose, noun, body_fluid).
lex(insulin, noun, hormone).
lex(betacell, noun, cell).
lex(pancreas, noun, organ).
lex(concentration, noun, quality).
lex(behaviour, noun, process).

% Lexicon: verbs
% lex(word:verb, type:transitivity, ontology: constraints).
lex(observe, trans, const(animate, entity)).
lex(produce, trans, const(process, body_fluid)).
lex(produce, trans, const(material, body_fluid)).

lex(aim, trans, const(artifact, function)). % Entities with function
lex(aim, trans, const(body_part, function)). % Entities with function
lex(is, trans, const(X,X)).
lex(smile, intrans, const(animate)).

% Lexicon: adjectives
% lex(word:adj, type:adj, ontology:constraints, semantic:type).
lex(red, adj, const(material), color).
lex(young, adj, const(animate), time).
lex(synchronous, adj, const(process), manner).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Defining transitive rules for constraints.
lex(X, trans, const(Y,M))  :-  isa(Y,Z), lex(X, trans, const(Z,M)), !.
lex(X, trans, const(Y,M))  :-  isa(M,N), lex(X, trans, const(Y,N)), !. % FIXME
lex(X, intrans, const(Y))  :-  isa(Y,Z), lex(X, intrans, const(Z)), !.
lex(X, adj, const(Y), M)   :-  isa(Y,Z), lex(X, adj, const(Z), M), !.

isa(X,Y)                   :-  isa(X,Y,[]).
isa(X,Y,V)		   :-  is_subset_of(X,Y), \+ memberchk((X,Y),V).
isa(X,Y,V)		   :-  is_subset_of(X,Z), \+ memberchk((X,Z),V),
			       isa(Z,Y,[(X,Y)|V]).

% Defining affinity rules for prepositional phrases
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

aff(P,Y,S2,R)              :-  isa(Y,S1), aff(P,S1,S2,R).
aff(P,S1,Y,R)              :-  isa(Y,S2), aff(P,S1,S2,R).

% Defining affinities for compound nouns (production related)
aff(cn, body_fluid, process, result).
aff(cn, organ, process, agent).
aff(cn, cell, process, agent).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Ontology skeletons from Basic Formal Ontology (BFO).
is_subset_of(continuant, entity).
is_subset_of(occurent, entity).

is_subset_of(independent_continuant, continuant).
is_subset_of(material, independent_continuant).
is_subset_of(immaterial, independent_continuant).

% is_subset_of(gen_dependent_continuant, continuant).
% is_subset_of(information, gen_dependent_continuant).
% is_subset_of(gene_sequence, gen_dependent_continuant).

is_subset_of(spe_dependent_continuant, continuant).
is_subset_of(quality, spe_dependent_continuant).
is_subset_of(color, quality).
is_subset_of(size, quality).

% is_subset_of(function, spe_dependent_continuant).
% is_subset_of(role, spe_dependent_continuant).
% is_subset_of(disposition, function).

is_subset_of(process, occurent).
is_subset_of(creation, process).
is_subset_of(transformation, process).
is_subset_of(event, occurent).
% is_subset_of(state, occurent).
% is_subset_of(presence, state).
% is_subset_of(lack, state).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Domain specific ontology specification.
is_subset_of(animate, material).
is_subset_of(human, animate).

is_subset_of(inanimate, material).
is_subset_of(furniture, inanimate).
% is_subset_of(artifact, inanimate).
is_subset_of(body_part, inanimate).
is_subset_of(organ, body_part).
is_subset_of(cell, body_part).

is_subset_of(body_fluid, inanimate).
is_subset_of(hormone, body_fluid).
is_subset_of(insulin, hormone).
% is_subset_of(protein, body_fluid).

% is_subset_of(immaterial, independent_continuant).
% is_subset_of(cognitive, immaterial).
% is_subset_of(idea, cognitive).

% LIMITS:
% ?- time(np(X,Z,[synchronous,synchronous,glucose,synchronous,production],[])).
% 158,413 inferences, 0.016 CPU in 0.009 seconds (173% CPU, 10138432 Lips)
%  X = np(n(production), mod([adj([manner], synchronous), adj([manner],
% synchronous), cn([result], n(glucose)), adj([manner], synchronous)])),
% Z = const(creation) .
















