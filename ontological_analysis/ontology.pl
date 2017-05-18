% Constrainted Grammar.
proposition(s(NP,VP))           -->   np(NP, const(S)), vp(VP, const(S)).
vp(vp(V,N), const(S))           -->   rterm(V, const(S, C)), np(N, const(C)).
vp(vp(V), const(S))             -->   rterm(V, const(S)).

np(np(A, N), const(S))          -->   adj(A, const(S)), np(N, const(S)).
np(np(N, mod(M)), const(S1))    -->   n(N, const(S1)), mod(M, const(S1)).
np(np(N), const(S))             -->   n(N, const(S)).

mod([pp(P,[R],N)|T], const(S1)) -->   prep(P, const(S1, S2), R), np(N, const(S2)),
				      mod(T, const(S1)).
mod([rc(V,N)|T], const(S1))	-->   [that], rterm(V, const(S1,S2)), np(N, const(S2)),
	                              mod(T, const(S1)).
mod([pp(P,[R],N)], const(S1))   -->   prep(P, const(S1, S2), R), np(N, const(S2)).
mod([rc(V,N)], const(S1))       -->   [that], rterm(V, const(S1,S2)), np(N, const(S2)).

prep(P, const(S1, S2), R)       -->   [P], {lex(P, preposition)}, {aff(P, S1, S2, R)}.
n(N, const(S))                  -->   [N], {lex(N, noun, S)}.
adj(adj(A), const(S))           -->   [A], {lex(A, adj, const(S))}.
rterm(verb(V), const(S,C))      -->   [V], {lex(V, trans, const(S,C))}.
rterm(verb(V), const(S))        -->   [V], {lex(V, intrans, const(S))}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Lexicon: prepositions
lex(in, preposition).
lex(of, preposition).
lex(by, preposition).

% Lexicon: nouns
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

% Lexicon: verbs
lex(observe, trans, const(animate, entity)).
lex(produce, trans, const(process, body_fluid)).
lex(produce, trans, const(material, body_fluid)).

lex(aim, trans, const(artifact, function)). % Entities with function
lex(aim, trans, const(body_part, function)). % Entities with function
lex(is, trans, const(X,X)).
lex(smile, intrans, const(animate)).

% Lexicon: adjectives
lex(red, adj, const(material)).
lex(young, adj, const(animate)).
lex(synchronous, adj, const(process)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Defining transitive rules for constraints.
lex(X, trans, const(Y,M))  :-  isa(Y,Z), lex(X, trans, const(Z,M)).
lex(X, trans, const(Y,M))  :-  isa(M,N), lex(X, trans, const(Y,N)).
lex(X, intrans, const(Y))  :-  isa(Y,Z), lex(X, intrans, const(Z)).
lex(X, adj, const(Y))      :-  isa(Y,Z), lex(X, adj, const(Z)).

isa(X,Y)                   :-  is_subset_of(X,Y).
isa(X,Y)                   :-  is_subset_of(X,Z), isa(Z,Y).

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

