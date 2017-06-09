%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Unambiguous_grammar.pl:
%
%     Disambiguated version of Extended grammar for Natural Logics,
%	             based on onthological analysis.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- [reverse_grammar].          % Function to reverse parse trees.
:- [onthology_skeleton].       % Loading skeleton onthology.
:- [onthological_constraints]. % Loading constraints for disambiguation.
:- [insulin_onthology_lexicon].% Loading the lexicon.


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
% - prepositional verbs: NOT CONSIDERED
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
p([p(NP,VP)])	        -->   np(NP, const(S)), vp(VP, const(S)), {VP\=[_,_]}.
p([p(N1,VP),p(N2,VP)])  -->   np(N1, const(S)), [and], np(N2, const(S)),
	                      vp(VP, const(S)), {VP\=[_,_]}.
p([p(NP,V1),p(NP,V2)])  -->   np(NP, const(S)), vp(VP, const(S)), {VP=[V1,V2]}.
p([p(N1,V1),p(N1,V2),
   p(N2,V1),p(N2,V2)])  -->   np(N1, const(S)), [and], np(N2, const(S)),
                              vp(VP, const(S)), {VP=[V1,V2]}.

vp(vp(V,pred(A)), const(S)) -->  rterm(V,_,copular, const(_,_)), adjs(A, const(S)).
vp(vp(V,N),	  const(S)) -->	 rterm(V,trans,_, const(S,C)),	np(N, const(C)).
vp(vp(verb(passive(V),mod(M))), const(S)) --> [is,V],
	{lex(_,trans, _,V,const(S,_))}, pps(M).
vp([vp(V,N1),vp(V,N2)], const(S)) -->   rterm(V,trans,_, const(S,C)), np(N1, const(C)),
	[and], np(N2, const(C)).
vp(vp(V,N), const(S)) --> rterm(V,trans,_,const(S,C)), np(N1, const(C)), [or],
	np(N2, const(C)), !, {reverse_np(N1,S1),reverse_np(N2,S2), supremum(S1,S2,N)}.

% Adjectives as predicate.
adjs([A],   const(S))	  -->  adj(A,predi,const(S)). % Always an adjective at least.
adjs([A|T], const(S))	  -->  adj(A,predi, const(S)), adjs(T, const(S)).

% Concept nouns accepting modifiers.
np(np(N,mod([]),ext([])), const(S)) -->	n(N, const(S)). % Kept for computational reasons.
np(np(N,mod(M),ext(E)),  const(S))  -->	pre(Q, const(S)), n(N, const(S)),
	                                post(P,E, const(S)), {append(Q,P,M)}.

% Defining pre-modifiers.
pre(T,           const(S))     -->  pre1(T, const(S)). % Case without possessive.
pre([G|T],       const(_))     -->  cn(N1, const(_)), ['s'], pre1(T),
	                            {G = ger([affiliation],N1)}.
pre1(T,          const(S))     -->  pre2(T, const(S)).
pre1([adj(A)|T], const(S))     -->  adj(A,_, const(S)), pre1(T, const(S)).
pre2([],	 const(_))     -->  {true}. % Potentially, no pre-modifiers.
pre2([cn([R],N1)|T], const(S)) -->  n(N1, const(C)), pre2(T, const(S)), {aff(cn,C,S,R)}.

% To allow compound nouns in possessives.
cn(np(N,mod([]),ext([])), const(S)) -->  n(N, const(S)).
cn(np(N,mod(M),ext([])),  const(S)) -->	 pre1(M, const(S)), n(N, const(S)).

% Defining post-modifiers.
post(M,[],	      const(S)) --> post1(M, const(S)). % Case without apposition.
post(M,[A],           const(S)) --> app(A), post1(M, const(S)).

post1([],             const(_)) --> {true}. % Potentially, no post-modifiers.

post1([rc(V,NP)],     const(S)) --> [that], rterm(V,trans,_, const(S,C)),
	                            np(NP, const(C)),
				    {reverse_rterm(V,Vs), Vs \= [isa]}.
post1([pp(P,[R],NP)], const(S)) --> prep(P, const(S,C),R), np(NP, const(C)).
post1([rc(V,NP)|M],   const(S)) --> [that], rterm(V,trans,_, const(S,C)),
	                            np(NP, const(C)), align(_), post1(M, const(S)),
                                    {reverse_rterm(V,Vs), Vs \= [isa]}.

post1([rc(V,NP)|M], const(S))   --> [that], rterm(V,trans,_, const(S,C)),
	                            np(NP, const(C)), post1(M, const(S)),
				    {reverse_rterm(V,Vs), Vs \= [isa]}.

post1([pp(P,[R],NP)|M], const(S)) -->  prep(P,const(S,C),R), np(NP, const(C)),
	                               align(_), post1(M, const(S)).
post1([pp(P,[R],NP)|M], const(S)) -->  prep(P,const(S,C),R), np(NP, const(C)),
	                               post1(M, const(S)).

% Defining appositions.
app(ap(N))         --> [','], [a], np(N), [','].
app(ap(N))         --> [','], [an], np(N), [','].

% Defining parenthetical clauses.
app(pc(V,N))       --> [','], [which], rterm(V,trans,_), np(N), [','].

% Relation terms
rterm(verb(V,mod(M)),X,T, const(S,C)) -->  rterm1(V,X,T, const(S,C)),
		    advs(P), pps(Q), {append(P,Q,M)}.

rterm(verb(passive(V),mod([])),X,T, const(S,C)) --> [is,V],
	       {lex(_,X,T,V, const(S,C))}, [by].
rterm(verb(passive(V),mod(M)),X,T,  const(S,C))	--> [is,V],
	       {lex(_,X,T,V, const(S,C))}, pps(M), [by].

rterm1(active(V),X,T,const(S,C)) --> [V], {lex(V, X, T, _, const(S,C))}.
% TODO - prepositional verb.

% Adverbial PPs.
pps([])		    -->   {true}. % Potentially no adverbial PPs.
pps([pp(P,N)])      -->	  prep(P, const(_,C), _), np(N, const(C)).
pps([pp(P,N)|T])    -->   prep(P, const(_,C), _), np(N, const(C)), align(_), pps(T).
pps([pp(P,N)|T])    -->   prep(P, const(_,C), _), np(N, const(C)), pps(T).

% Adverbs.
advs([])		--> {true}. % Potentially no adverbs.
advs([A|T])	        --> adv(A), advs(T).

% Alignment keywords.
align(X)                -->   [X], {X = and}.
align(X)	        -->   [X], {X = ','}.

% Access to lexicon entries
prep(P,  const(S,C), R) -->   [P], {lex(P, preposition), aff(P,S,C,R)}.
n(n(N),  const(S))      -->   [N], {lex(N, noun, S)}.
adj(A,T, const(S))      -->   [A], {lex(A, adj, _, T, const(S))}.
adv(A)		        -->   [A], {lex(A, adv)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

supremum(S1,S2,N) :- ( isa(S1,N), isa(S2,N) -> print_common(N,S1,S2) ; fail).

print_common(N,S1,S2) :- nl, write('Common supremum found for '), write(S1),
	                 write(' and '), write(S2), write(' with '), write(N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%










