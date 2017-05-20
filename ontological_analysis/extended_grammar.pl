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
proposition(s(NP,VP)) -->   np(NP), vp(VP).
vp(vp(V,N))           -->   rterm(V), np(N).
vp(vp(V))             -->   rterm(V).

np(np0(N))	      -->   n(N).
% np(np(N, mod(M)))     -->   n(N), post(M).
% np(np(N, mod(M)))     -->   pre(M), n(N).
np(np1(N, mod(M)))     -->   pre(Q), n(N), post(P), {append(Q,P,M)}.

% np(np(N, mod(M)))     -->   cn(N1), ['s'], n(N), post(P),
%			    {M = [ger([affiliation], N1)|P]}.
% np(np(N, mod(M)))     -->   cn(N1), ['s'], pre(Q), n(N),
%			    {M = [ger([affiliation], N1)|Q]}.
np(np2(N, mod(M)))     -->   cn(N1), ['s'], pre(Q), n(N), post(P),
			    {append([ger([affiliation], N1)|Q],P,M)}.

% To allow compound nouns in possessives.
cn(np(N))             -->   n(N).
cn(np(N, mod(M)))     -->   pre(M), n(N).

% Defining pre-modifiers
pre([adj(A)])     -->   adj(A).
pre([cn(N1)])     -->   n(N1).
pre([adj(A)|T])   -->   adj(A), pre(T).
pre([cn(N1)|T])   -->   n(N1), pre(T).
pre([])	          -->   {true}.


% Defining post-modifiers
post([pp(P,N)])   -->   prep(P), np(N).
post([rc(V,N)])   -->   [that], rterm(V), np(N).
post([pp(P,N)|T]) -->   prep(P), np(N), align(_), post(T).
post([pp(P,N)|T]) -->   prep(P), np(N), post(T).
post([rc(V,N)|T]) -->   [that], rterm(V), np(N), align(_), post(T).
post([rc(V,N)|T]) -->   [that], rterm(V), np(N), post(T).
post([])	  -->   {true}.

prep(P)           -->   [P], {lex(P, preposition)}.
n(n(N))	          -->   [N], {lex(N, noun)}.
adj(A)	          -->   [A], {lex(A, adj)}.

rterm(verb(isa))  -->   [isa].
rterm(verb(V))    -->   [V], {lex(V, trans)}.
rterm(verb(V))    -->   [V], {lex(V, intrans)}.

align(X)          -->   [X], {X = and}.
align(X)	  -->   [X], {X = ','}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Lexicon: prepositions
lex(in, preposition).
lex(of, preposition).
lex(by, preposition).

% Lexicon: nouns
lex(doctor, noun).
lex(table, noun).
lex(disease, noun).
lex(production, noun).
lex(glucogenesis, noun).
lex(conversion, noun).
lex(glucose, noun).
lex(insulin, noun).
lex(betacell, noun).
lex(pancreas, noun).
lex(concentration, noun).
lex(behaviour, noun).

% Lexicon: verbs
lex(observe, trans).
lex(produce, trans).
lex(produce, trans).

lex(aim, trans). % Entities with function
lex(aim, trans). % Entities with function
lex(is, trans).
lex(smile, intrans).

% Lexicon: adjectives
lex(red, adj).
lex(young, adj).
lex(synchronous, adj).










