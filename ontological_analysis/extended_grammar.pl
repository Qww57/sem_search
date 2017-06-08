%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Extended_grammar.pl:
%
%	       Extanded grammar for Natural Logics, without
%	                 onthological analysis.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [reverse_grammar].          % Function to reverse parse trees.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
% - adverbs: OK
% - adverbial prepositional phrase: OK
% - expressing conditions: NOT CONSIDERED
% (on propositions)
% - conjunctions - distributive: OK
% - disjunctions - distributive: OK
%
%
% Passive RC without agent refused.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Propositions with plural formation.
p([p(NP,VP)])	        -->   np(NP), vp(VP), {VP\=[_,_]}.
p([p(N1,VP),p(N2,VP)])  -->   np(N1), [and], np(N2), vp(VP), {VP\=[_,_]}.
p([p(NP,V1),p(NP,V2)])  -->   np(NP), vp(VP), {VP=[V1,V2]}.
p([p(N1,V1),p(N1,V2),
   p(N2,V1),p(N2,V2)])  -->   np(N1), [and], np(N2), vp(VP), {VP=[V1,V2]}.

vp(vp(V,pred(A)))	-->   rterm(V,_,copular), adjs(A).
vp(vp(V,N))             -->   rterm(V,trans,_), np(N).
vp(vp(verb(passive(V),mod(M)))) --> [is,V], {lex(_,trans, _,V)}, pps(M).
vp([vp(V,N1),vp(V,N2)]) -->   rterm(V,trans,_), np(N1), [and], np(N2).
vp(vp(V,N))	        -->   rterm(V,trans,_), np(N1), [or], np(N2), !,
			      {reverse_np(N1,S1),reverse_np(N2,S2), supremum(S1,S2,N)}.

% Adjectives as predicate.
adjs([A])               -->  adj(A,predi). % Always an adjective at least.
adjs([A|T])		-->  adj(A,predi), adjs(T).

% Concept nouns accepting modifiers.
np(np(N,mod([]),ext([]))) -->   n(N). % Kept for computational reasons.
np(np(N,mod(M),ext(E)))   -->   pre(Q), n(N), post(P,E), {append(Q,P,M)}. % sort(S,M)}.

% Defining pre-modifiers.
pre(T)             -->  pre1(T). % Case without possessive.
pre([G|T])	   -->  cn(N1), ['s'], pre1(T), {G = ger([affiliation],N1)}.
pre1(T)            -->  pre2(T).
pre1([adj(A)|T])   -->  adj(A,_), pre1(T).
pre2([])           -->  {true}. % Potentially, no pre-modifiers.
pre2([cn(N1)|T])   -->  n(N1), pre2(T).

% To allow compound nouns in possessives.
cn(np(N,mod([]),ext([]))) -->   n(N).
cn(np(N,mod(M),ext([])))  -->   pre1(M), n(N).

% Defining post-modifiers.
post(M,[])          -->  post1(M). % Case without apposition.
post(M,[A])	    -->  app(A), post1(M).

post1([])	    -->  {true}. % Potentially, no post-modifiers.

post1([rc(V,NP)]) --> [that], rterm(V,trans,_), np(NP),
                        {reverse_rterm(V,Vs), Vs \= [isa]}.				  post1([pp(P,NP)]) -->  prep(P), np(NP).

post1([rc(V,NP)|M]) --> [that], rterm(V,trans,_), np(NP), align(_), post1(M),
                        {reverse_rterm(V,Vs), Vs \= [isa]}.

post1([rc(V,NP)|M]) -->	[that], rterm(V,trans,_), np(NP), post1(M),
			{reverse_rterm(V,Vs), Vs \= [isa]}.

post1([pp(P,NP)|M]) -->  prep(P), np(NP), align(_), post1(M).
post1([pp(P,NP)|M]) -->  prep(P), np(NP), post1(M).

% Defining appositions.
app(ap(N))         --> [','], [a], np(N), [','].
app(ap(N))         --> [','], [an], np(N), [','].

% Defining parenthetical clauses.
app(pc(V,N))       --> [','], [which], rterm(V,trans,_), np(N), [','].

% Relation terms
rterm(verb(V,mod(M)),X,T) -->   rterm1(V,X,T), advs(P), pps(Q), {append(P,Q,M)}.

rterm(verb(passive(V),mod([])),X,T) -->	[is,V], {lex(_,X,T,V)}, [by].
rterm(verb(passive(V),mod(M)),X,T)  -->	[is,V], {lex(_,X,T,V)}, pps(M), [by].

rterm1(active(V),X,T)     -->   [V], {lex(V, X, T, _)}. % Active transitive.
rterm1(active(V,P),X,T)   -->   [V], {lex(V, X, T, _)}, prep(P). % Act intrans with prep.

% Adverbial PPs.
pps([])		    -->   {true}. % Potentially no adverbial PPs.
pps([pp(P,N)])      -->	  prep(P), np(N).
pps([pp(P,N)|T])    -->   prep(P), np(N), align(_), pps(T).
pps([pp(P,N)|T])    -->   prep(P), np(N), pps(T).

% Adverbs.
advs([])        --> {true}. % Potentially no adverbs.
advs([A|T])	--> adv(A), advs(T).

% Alignment keywords.
align(X)          -->   [X], {X = and}.
align(X)	  -->   [X], {X = ','}.

% Access to lexicon entries
prep(P)            -->   [P], {lex(P, preposition)}.
n(n(N))	           -->   [N], {lex(N, noun)}.
adj(A,T)           -->   [A], {lex(A, adj, _, T)}.
adv(A)		   -->	 [A], {lex(A, adv)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

supremum(S1,S2,N) :- ( isa(S1,N), isa(S2,N) -> print_common(N,S1,S2) ; fail).

print_common(N,S1,S2) :- nl, write('Common supremum found for '), write(S1),
	                 write(' and '), write(S2), write(' with '), write(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
















