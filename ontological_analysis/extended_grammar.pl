%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Extensions of Natural Logics:
% (on noun phrases)
% - relative clauses: OK (post)
% - prepositional phrases: OK (post)
% - compound nouns: OK (pre)
% - possessives: OK (pre)
% - adjectives: OK (pre)
% - appositions: OK (post)
% - fully recursive structures: OK (order and composition)
% (on verbs)
% - prepositional verbs: OK
% - passive forms: OK
% - passivisation: NOT IN PARSER
% - nominalisation: NOT IN PARSER
% - adverbs: OK
% - adverbial prepositional phrase: OK
% - expressing conditions: NOT CONSIDERED
% (on propositions)
% - conjonctions - respective: OK -> to remove
% - conjunctions - distributive: OK
% - disjunctions - distributive: OK
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Propositions with plural formation.
p([p(NP,VP)])	        -->   np(NP), vp(VP), {VP\=[_,_]}.
p([p(N1,VP),p(N2,VP)])  -->   np(N1), [and], np(N2), vp(VP), {VP\=[_,_]}.
p([p(NP,V1),p(NP,V2)])  -->   np(NP), vp(VP), {VP=[V1,V2]}.
p([p(N1,V1),p(N1,V2),
   p(N2,V1),p(N2,V2)])  -->   np(N1), [and], np(N2), vp(VP), {VP=[V1,V2]}.

vp(vp(V,pred(A)))	-->   rterm(V,_,copular), adjs(A).
vp(vp(V,N))             -->   rterm(V,trans,_), np(N).
vp([vp(V,N1),vp(V,N2)]) -->   rterm(V,trans,_), np(N1), [and], np(N2).
vp(vp(V,N))	        -->   rterm(V,trans,_), np(N1), [or], np(N2), !,%TO FIX FOR PLURAL
			     {np(N1,S1,[]), np(N2,S2,[]), !, isa(S1,N), isa(S2,N)},
			     {print_common(N,S1,S2)}.

% Adjectives as predicate.
adjs([A])               -->  adj(A,predi). % Always an adjective at least.
adjs([A|T])		-->  adj(A,predi), adjs(T).

% Concept nouns accepting modifiers.
np(np(N,mod([]),ext([]))) -->   n(N). % Kept for computational reasons.
np(np(N,mod(M),ext(E)))   -->   pre(Q), n(N), post(P,E), {append(Q,P,S), sort(S,M)}.

% Defining pre-modifiers.
pre(T)             -->  pre1(T). % Case without possessive.
pre([G|T])	   -->  cn(N1), ['s'], pre1(T), {G = ger([affiliation],N1)}.
pre1([adj(A)|T])   -->  adj(A,_), pre1(T).
pre1(T)            -->  pre2(T).
pre2([cn(N1)])     -->  n(N1). % Not needed, for efficiency reasons.
pre2([cn(N1)|T])   -->  n(N1), pre2(T).
pre2([])           -->  {true}.

% To allow compound nouns in possessives.
cn(np(N))          -->   n(N).
cn(np(N, mod(M)))  -->   pre1(M), n(N).

% Defining post-modifiers.
post(M,[])         -->   post1(M). % Case without apposition.
post(M,[A])	   -->   app(A), post1(M).
post1([pp(P,N)])   -->   prep(P), np(N).
post1([rc(V,N)])   -->   [that], rterm(V,trans,_), np(N). % ADD INTRANS
post1([pp(P,N)|M]) -->   prep(P), np(N), align(_), post1(M).
post1([pp(P,N)|M]) -->   prep(P), np(N), post1(M).
post1([rc(V,N)|M]) -->   [that], rterm(V,trans,_), np(N), align(_), post1(M).
post1([rc(V,N)|M]) -->   [that], rterm(V,trans,_), np(N), post1(M).
post1([])	   -->   {true}.

% Defining appositions.
app(ap(N))         --> [','], [a], np(N), [','].
app(ap(N))         --> [','], [an], np(N), [','].

% Defining parenthetical clauses.
app(pc(V))         --> [','], [which], rterm(V,_,_),[','].
app(pc(V,N))       --> [','], [which], rterm(V,trans,_), np(N), [','].

% Relation terms
rterm(verb(V,mod(M)),X,T) -->   rterm1(V,X,T), advs(P), pps(Q), {append(P,Q,M)}.
rterm(verb(V),X,T)        -->	[is], [V], {lex(_, X, T, V)}, [by]. % Passive trans.
rterm(verb(V,mod(M)),X,T) -->	[is], [V], {lex(_, X, T, V)}, pps(M), [by].

rterm1(active(isa),X,T)   -->   [isa], {T = copular, X = trans}.
rterm1(active(V),X,T)	  -->   [V], {lex(V, X, T)}. % Active intransitive.
rterm1(active(V),X,T)     -->   [V], {lex(V, X, T, _)}. % Active transitive.
rterm1(active(V,P),X,T)   -->   [V], {lex(V, X, T, _)}, prep(P). % Act intrans with prep.

% Adverbial PPs
pps([pp(P,N)])      -->	  prep(P), np(N).
pps([pp(P,N)|T])    -->   prep(P), np(N), align(_), pps(T).
pps([pp(P,N)|T])    -->   prep(P), np(N), pps(T).
pps([])		    -->   {true}.

% Adverbs
advs([A|T])	--> adv(A), advs(T).
advs([])        --> {true}. % Potentially no adverb

% Access to lexicon entries
prep(P)            -->   [P], {lex(P, preposition)}.
n(n(N))	           -->   [N], {lex(N, noun)}.
adj(A,T)           -->   [A], {lex(A, adj, _, T)}.
adv(A)		   -->	 [A], {lex(A, adv)}.

align(X)          -->   [X], {X = and}.
align(X)	  -->   [X], {X = ','}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


print_common(N,S1,S2) :- write('Common supremum found for '), write(S1), write(' and '),
			 write(S2), write(' with '), write(N).

% Nominalization approach:
% - subject of active as Agent (by) and Object as patient (of).
% - subject of passive as patient (of) and Object as agent (by).
% - keep adverbial PP as nominal PP.
% - turn adverbs to adjectives
% nominalization(P,NP) :- p(X,P,[]), write('TODO').

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

