% Grammar.
proposition(s(NP,VP))           -->   np(NP, const(S)), vp(VP, const(S)).
vp(vp(V,N), const(S))           -->   rterm(V, const(S, C)), np(N, const(C)).
vp(vp(V), const(S))             -->   rterm(V, const(S)).

np(np(A, N), const(S))          -->   adj(A, const(S)), np(N, const(S)).
np(np(N, mod(M)), const(S1))    -->   n(N, const(S1)), mod(M, const(S1)).
np(np(N), const(S))             -->   n(N, const(S)).

mod([pp(P,[R],N)|T], const(S1)) -->   prep(P, const(S1, S2), R), np(N, const(S2)),
				      mod(T, const(S1)).
mod([pp(P,[R],N)], const(S1))   -->   prep(P, const(S1, S2), R), np(N, const(S2)).

prep(P, const(S1, S2), R)       -->   [P], {lex(P, preposition)}, {aff(P, S1, S2, R)}.
n(N, const(S))                  -->   [N], {lex(N, noun, S)}.
adj(adj(A), const(S))           -->   [A], {lex(A, adj, const(S))}.
rterm(verb(V), const(S,C))      -->   [V], {lex(V, trans, const(S,C))}.
rterm(verb(V), const(S))        -->   [V], {lex(V, intrans, const(S))}.

% Lexicon: nouns
% lex(doctor, noun, human).
% lex(table, noun, furniture).
lex(disease, noun, process).
lex(production, noun, process).
lex(glucose, noun, material).
lex(cell, noun, material).
lex(pancreas, noun, material).
lex(concentration, noun, quality).

% Lexicon: verbs
% lex(observe, trans, const(animate, entity)).
% lex(is, trans, const(X,X)).
% lex(smile, intrans, const(animate)).

% Lexicon: adjectives
lex(red, adj, const(material)).
% lex(young, adj, const(animate)).
lex(synchronous, adj, const(process)).

% Defining transitive rules for constraints.
% lex(X, trans, T, const(Y,M)) :- isa(Y,Z), lex(X, trans, T,
% const(Z,M)). lex(X, trans, T, const(Y,M)) :- isa(M,N), lex(X, trans,
% T, const(Y,N)). lex(X, intrans, T, const(Y)) :- isa(Y,Z), lex(X,
% intrans, T, const(Z)). lex(X, adj, T, const(Y)) :- isa(Y,Z), lex(X,
% adj, T, const(Z)).

% isa(X,Y) :- is_subset_of(X,Y).
% isa(X,Y) :- is_subset_of(X,Z), isa(Z,Y).

% Defining preposition and their semantic relation.
lex(in, preposition).
lex(of, preposition).
lex(by, preposition).

% Defining affinity rules for prepositional phrases
aff(in, material, material, location).
aff(in, process, material, location).
% aff(in, quality, material, beared_by).
% aff(in, information, process, affiliation).

aff(by, process, material, agent).
aff(by, process, process, manner).
aff(by, process, artifact, instrument).

aff(of, process, material, result). % Replace by creation
% aff(of, transformation, material, patient).
% aff(of, process, material, beared_by).
% aff(of, process, process, has_part).
% aff(of, material, material, part_of).

% Ontology skeletons from Basic Formal Ontology (BFO).
% is_subset_of(continuant, entity).

% is_subset_of(independent_continuant, continuant).
% is_subset_of(material, independent_continuant).
% is_subset_of(immaterial, independet_continuant).

% is_subset_of(gen_dependent_continuant, continuant).
% is_subset_of(information, gen_dependent_continuant).
% is_subset_of(gene_sequence, gen_dependent_continuant).
% is_subset_of(spe_dependent_continuant, continuant).
% is_subset_of(non_realizable_dep_continuant, spe_dependent_continuant).
% is_subset_of(quality, non_realizable_dep_continuant).
% is_subset_of(color, quality).
% is_subset_of(realizable_dep_continuant, spe_dependent_continuant).
% is_subset_of(function, realizable_dep_continunant).
% is_subset_of(role, realizable_dep_continunant).
% is_subset_of(disposition(_,_) , realizable_dep_continunant).
% is_subset_of(capability, realizable_dep_continunant).
% is_subset_of(tendency, realizable_dep_continunant).

% is_subset_of(occurent, entity).
% is_subset_of(process, occurent).
% is_subset_of(event, occurent).
% is_subset_of(realization, process).
% is_subset_of(curse_of_disease, realization).
% is_subset_of(execution, realization).
% is_subset_of(expression, realization).
% is_subset_of(application, realization).

% Domain specific ontology specification.
% is_subset_of(animate, material).
% is_subset_of(human, animate).

% is_subset_of(inanimate, thing).
% is_subset_of(furniture, inanimate).
% is_subset_of(artifact, inanimate).
% is_subset_of(body_part, inanimate).







