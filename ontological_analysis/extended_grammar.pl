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
% - appositions: OK (post)
% (on verbs)
% - prepositional verbs: OK
% - passive forms: OK
% - passivisation: NOT IN PARSER
% - nominalisation: NOT IN PARSER
% - adverbs: TODO
% - adverbial prepositional phrase: TODO
% - expressing conditions: TODO
% (on propositions)
% - conjonctions - respective: OK
% - conjunctions - distributive: OK
% - disjunctions - distributive: OK
%
% Improvements:
% - refactoring relation terms.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Propositions with plural formation.
p([p(NP,VP)])	       -->   np(NP), vp(VP), {VP\=[_,_]}.
p([p(N1,VP),p(N2,VP)]) -->   np(N1), [and], np(N2), vp(VP), {VP\=[_,_]}.
p([p(NP,V1),p(NP,V2)]) -->   np(NP), vp(VP), {VP=[V1,V2]}.
p([p(N1,V1),p(N2,V2)]) -->   np(N1), [and], np(N2), vp(VP), {VP=[V1,V2]}.

vp(vp(V))	       -->   rterm(V,_,_).
vp(vp(V,A))            -->   rterm(V,_, copular), adj(A, predi).
vp(vp(V,N))            -->   rterm(V,trans,_), np(N).
vp([vp(V,N1),vp(V,N2)])-->   rterm(V,trans,_), np(N1), [and], np(N2).
vp(vp(V,N))	       -->   rterm(V,trans,_), np(N1), [or], np(N2), !,
			     {np(N1,S1,[]), np(N2,S2,[]), isa(S1,N), isa(S2,N)},
			     {print_common(N,S1,S2)}.

% Concept nouns accepting modifiers.
np(np(N))	       -->   n(N). % Kept for computational reasons.
np(np(N, mod(M)))      -->   pre(Q), n(N), post(P), {append(Q,P,S), sort(S,M)}.

% To allow compound nouns in possessives.
cn(np(N))              -->   n(N).
cn(np(N, mod(M)))      -->   pre1(M), n(N).

% Defining pre-modifiers.
pre(T)             -->  pre1(T). % Case without possessive.
pre([G|T])	   -->  cn(N1), ['s'], pre1(T), {G = ger([affiliation],N1)}.

pre1([adj(A)])     -->  adj(A,_).
pre1([cn(N1)])     -->  n(N1).
pre1([adj(A)|T])   -->  adj(A,_), pre1(T).
pre1([cn(N1)|T])   -->  n(N1), pre1(T).
pre1([])           -->  {true}.

% Defining post-modifiers.
post(T)            -->   post1(T). % Case without apposition.
post([A|T])	   -->   app(A), post1(T).

post1([pp(P,N)])   -->   prep(P), np(N).
post1([rc(V,N)])   -->   [that], rterm(V,trans,_), np(N).
post1([pp(P,N)|T]) -->   prep(P), np(N), align(_), post1(T).
post1([pp(P,N)|T]) -->   prep(P), np(N), post1(T).
post1([rc(V,N)|T]) -->   [that], rterm(V,trans,_), np(N), align(_), post1(T).
post1([rc(V,N)|T]) -->   [that], rterm(V,trans,_), np(N), post1(T).
post1([])	   -->   {true}.

% Defining appositions.
app(ap(N))   --> [','], [a], np(N), [','].
app(ap(N))   --> [','], [an], np(N), [','].

% Defining parenthetical clauses.
app(pc(V))   --> [','], [which], rterm(V,_,_),[','].
app(pc(V,N)) --> [','], [which], rterm(V,trans,_), np(N), [','].

% Adverbial PPs - to do
pp([pp(P,N)])     -->	prep(P), np(N).
pp([pp(P,N)|T])   -->   prep(P), np(N), align(_), pp(T).
pp([pp(P,N)|T])   -->   prep(P), np(N), pp(T).

% Access to lexicon entries
prep(P)           -->   [P], {lex(P, preposition)}.
n(n(N))	          -->   [N], {lex(N, noun)}.
adj(A,T)          -->   [A], {lex(A, adj, _, T)}.

rterm(verb(isa), X, T)	-->   [isa], {T = copular, X = trans}.
rterm(verb(V), X, T)	-->   [V], {lex(V, X, T)}. % Active intransitive.
rterm(verb(V), X, T)    -->   [V], {lex(V, X, T, _)}. % Active transitive.
rterm(verb(V,P), X, T)	-->   [V], {lex(V, X, T, _)}, prep(P). % Active intrans with prep.
rterm(rpas(V,P), X, T)  -->   [is],[V], {lex(_, X, T, V)}, prep(P). % Passive transitive.

align(X)          -->   [X], {X = and}.
align(X)	  -->   [X], {X = ','}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


print_common(N,S1,S2) :- write('Common supremum found for '), write(S1), write(' and '),
			 write(S2), write(' with '), write(N).


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
lex(alphacell, noun).
lex(betacell, noun).
lex(cell, noun).
lex(pancreas, noun).
lex(concentration, noun).
lex(behaviour, noun).

% Lexicon: verbs
% Distinction between 'effect, copular and stat'
lex(smile, intrans, effect).
lex(is, trans, copular).
lex(observe, trans, effect, observed).
lex(produce, trans, effect, produced).
lex(reside, trans, state, hosted).

% Lexicon: adjectives
lex(red, adj, subs, predi).
lex(young, adj, subs, predi).
lex(synchronous, adj, inter, predi).
lex(possible, adj, non_subs, non_predi).
lex(former, adj, privative, non_predi).

% ISA relations
isa([betacell], [cell]).
isa([alphacell], [cell]).

