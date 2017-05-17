% Grammar.
prop(s(NP,VP))     -->   np(NP), vp(VP).
vp(vp(V,N))        -->   rterm(V), np(N).
vp(vp(V))          -->   rterm(V).

np(np(A, N))	   -->   adj(A), np(N).
np(np(N, mod(M)))  -->   n(N), mod(M).
np(np(N))          -->   n(N).

mod([pp(P,N)|T])   -->   prep(P), np(N), mod(T).
mod([pp(P,N)])     -->   prep(P), np(N).

prep(P)            -->   [P], {lex(P, preposition)}.
n(N)               -->   [N], {lex(N, noun, _)}.
adj(adj(A))        -->   [A], {lex(A, adj, _)}.
rterm(verb(V))     -->   [V], {lex(V, trans, _)}.
rterm(verb(V))     -->   [V], {lex(V, intrans, _)}.

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

lex(in, preposition).
lex(of, preposition).
lex(by, preposition).

