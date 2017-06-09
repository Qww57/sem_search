%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Unambiguous_grammar.pl:
%
%     Disambiguated version of Extended grammar for Natural Logics,
%	             based on onthological analysis.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- ensure_loaded([onthology_skeleton]).        % Loading ontology.
:- ensure_loaded([onthological_constraints]).  % Loading constraints.
:- [insulin_onthology_lexicon].                % Loading the lexicon.


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

vp(vp(V,pred(A)), const(S0)) -->  rterm(V,_,copular, const(_,_)), adjs(A, const(S1)),
				  {isa_of(S0,S1)}.
vp(vp(V,N),	  const(S0)) -->  rterm(V,trans,_, const(S1,S2)), np(N, const(S3)),
	                          {isa_of(S0,S1), isa_of(S3,S2)}.

vp(vp(verb(passive(V),mod(M))), const(S0)) --> [is,V],
	{lex(_,trans, _,V,const(S1,_))}, {isa_of(S0,S1)}, pps(M).
vp([vp(V,N1),vp(V,N2)], const(S0)) -->  rterm(V,trans,_, const(S1,S2)),
	np(N1, const(S3)), [and], np(N2, const(S4)),
	{isa_of(S0,S1), isa_of(S3,S2), isa_of(S4,S2)}.
%  vp(vp(V,N), const(S)) --> rterm(V,trans,_,const(S,C)), np(N1,const(C)), [or],
% np(N2, const(C)), !,{reverse_np(N1,S1),reverse_np(N2,S2), supremum(S1,S2,N)}.

% Adjectives as predicate.
adjs([A],   const(S0))	  -->  adj(A,predi, const(S0)).
adjs([A|T], const(S0))	  -->  adj(A,predi, const(S0)), adjs(T,const(S0)).

% Concept nouns accepting modifiers.
np(np(N,mod([]),ext([])), const(S0)) --> n(N, const(S0)).
np(np(N,mod(M),ext(E)),   const(S0)) --> pre(Q, const(S0)), n(N, const(S0)),
	                                 post(P,E, const(S0)), {append(Q,P,M)}.

% Defining pre-modifiers.
pre(T,           const(S0))     -->  pre1(T, const(S0)). % Case without possessive.
pre([G|T],       const(S0))     -->  cn(N1, const(_)), ['s'], pre1(T, const(S0)),
	                             {G = ger([affiliation],N1)}.
pre1(T,          const(S0))     -->  pre2(T, const(S0)).
pre1([adj(A)|T], const(S0))     -->  adj(A,_,const(S1)), {write(S1), write(S0), nl},
				     pre1(T,const(S0)),
	                             {isa_of(S0,S1)}.
pre2([],	 const(_))      -->  {true}. % Potentially, no pre-modifiers.

% TODO
pre2([cn([affilitation],N1)|T], const(S0)) -->  n(N1, const(_)), pre2(T, const(S0)).
				     % {aff(cn,S0, S1,R)}.


% To allow compound nouns in possessives.
cn(np(N,mod([]),ext([])), const(S0)) -->  n(N, const(S0)).
cn(np(N,mod(M),ext([])),  const(S0)) -->  pre1(M, const(S0)), n(N, const(S0)).

% Defining post-modifiers.
post(M,[],	      const(S0)) --> post1(M, const(S0)). % Case without apposition.
post(M,[A],           const(S0)) --> app(A, const(S0)), post1(M, const(S0)).

post1([],             const(_)) --> {true}. % Potentially, no post-modifiers.

post1([rc(V,NP)],     const(S0)) --> [that], rterm(V,trans,_, const(L0,L1)),
	                            np(NP, const(S1)),
				    {reverse_rterm(V,Vs), Vs \= [isa]},
				    {isa_of(S0,L0), isa_of(S1,L1)}.
post1([pp(P,[R],NP)], const(S)) --> prep(P, const(S,C),R), np(NP, const(C)).
post1([rc(V,NP)|M],   const(S0)) --> [that], rterm(V,trans,_, const(L0,L1)),
	                            np(NP, const(S1)), align(_), post1(M, const(S0)),
                                    {reverse_rterm(V,Vs), Vs \= [isa],
				     isa_of(S0,L0), isa_of(S1,L1)}.

post1([rc(V,NP)|M], const(S0))   --> [that], rterm(V,trans,_, const(L0,L1)),
	                            np(NP, const(S1)), post1(M, const(S0)),
				    {reverse_rterm(V,Vs), Vs \= [isa],
				    isa_of(S0,L0), isa_of(S1,L1)}.

post1([pp(P,[R],NP)|M], const(S)) -->  prep(P,const(S,C),R), np(NP, const(C)),
	                               align(_), post1(M, const(S)).
post1([pp(P,[R],NP)|M], const(S)) -->  prep(P,const(S,C),R), np(NP, const(C)),
	                               post1(M, const(S)).

% Defining appositions. % TODO
app(ap(N), const(_))  --> [','], [a],  np(N, const(_)), [','].
app(ap(N), const(_))  --> [','], [an], np(N, const(_)), [','].

% Defining parenthetical clauses.
app(pc(V,N), const(S0)) --> [','], [which], rterm(V,trans,_,const(L0,L1)),
	np(N,const(S1)), [','], {isa_of(S0,L0), isa_of(S1,L1)}.

% Relation terms
rterm(verb(V,mod(M)),X,T, const(S,C)) -->  rterm1(V,X,T, const(S,C)),
		    advs(P), pps(Q), {append(P,Q,M)}.

rterm(verb(passive(V),mod([])),X,T, const(S,C)) --> [is,V],
	       {lex(_,X,T,V,const(S,C))}, [by].
rterm(verb(passive(V),mod(M)),X,T,  const(S,C))	--> [is,V],
	       {lex(_,X,T,V,const(S,C))}, pps(M), [by].

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
prep(P,  const(S0,S1), R) -->   [P], {lex(P, preposition), aff(P,S0,S1,R)}.
n(n(N),  const(S0))       -->   [N], {lex(N, noun, S0)}.
adj(A,T, const(L))        -->   [A], {lex(A, adj, _, T, const(L))}.
adv(A)		          -->   [A], {lex(A, adv)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining transitive rules for constraints.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% lex0(P, preposition)	     :-  lex(P, preposition).
% lex0(A, adv)		     :-  lex(A, adv).
% lex0(N, noun, S)             :-  lex(N, noun, S).
%  lex0(X, trans, const(Y,M)) :- isa(Y,Z), lex(X, trans, const(Z,M)), !.
% lex0(X, trans, const(Y,M)) :- isa(M,N), lex(X, trans, const(Y,N)), !.
%  lex0(X, adj, T, P, const(Y)) :- isa(Y,Z), lex(X, adj, T, P,
%  const(Z)), !.

isa(X,X).
isa(X,Y)    :-  isa(X,Y,[]).
isa(X,Y,V)  :-  is_subset_of(X,Y), \+ memberchk((X,Y),V).
isa(X,Y,V)  :-  is_subset_of(X,Z), \+ memberchk((X,Z),V), isa(Z,Y,[(X,Y)|V]).

% isa_of(X,L) - returns true if X is a subclass of one of the elements of L.
isa_of(_,[]) :- fail.
isa_of(X,[H|T]) :- (isa(X,H) -> true ; isa_of(X,T)).

aff(P,Y,S2,R)  :-  isa(Y,S1), aff(P,S1,S2,R).
aff(P,S1,Y,R)  :-  isa(Y,S2), aff(P,S1,S2,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

supremum(S1,S2,N) :- ( isa(S1,N), isa(S2,N) -> print_common(N,S1,S2) ; fail).

print_common(N,S1,S2) :- nl, write('Common supremum found for '), write(S1),
	                 write(' and '), write(S2), write(' with '), write(N).

reverse_rterm(verb(active(V),mod(_)), [V]).
reverse_rterm(verb(passive(V),mod(_)), [V]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%













