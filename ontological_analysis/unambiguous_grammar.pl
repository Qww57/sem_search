%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Unambiguous_grammar.pl:
%
%     Disambiguated version of Extended grammar for Natural Logics,
%	             based on onthological analysis.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- [onthology_skeleton].	   % Loading the ontology skeleton.
:- [onthological_constraints].	   % Loading the domain constraints.
:- [insulin_onthology_lexicon].    % Loading the dedicated lexicon.


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
% NOT IMPLEMENTED ON PRE MODIFIERS FOR NOW.
% MISSING DOUBLE DISTRIBUTIVE.
% MISSING PREPOSITIONAL VERB.
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
	{lex(V0,trans, _,V,const(_,SS0))},  {isa_of(S0,SS0)}, pps(M, V0).
vp([vp(V,N1),vp(V,N2)], const(S0)) -->  rterm(V,trans,_, const(S1,S2)),
	np(N1, const(S3)), [and], np(N2, const(S4)),
	{isa_of(S0,S1), isa_of(S3,S2), isa_of(S4,S2)}.
% vp(vp(V,N), const(S)) --> rterm(V,trans,_,const(S,C)), np(N1,const(C)), [or],
% np(N2, const(C)), !,{reverse_np(N1,S1),reverse_np(N2,S2), supremum(S1,S2,N)}.


% Adjectives as predicate.
adjs([A],   const(S0))	  -->  adj(A,predi, const(S0)).
adjs([A|T], const(S0))	  -->  adj(A,predi, const(S0)), adjs(T,const(S0)).


% Concept nouns accepting modifiers.
np(np(N,mod([]),ext([])), const(S0)) --> n(N, const(S0)).
np(np(N,mod(M),ext(E)),   const(S0)) --> pre(Q, L, const(S0)), n(N, const(S0)),
	                                 post(P,E, const(S0)),
					 {check_adj(S0,L), append(Q,P,M)}.

check_adj(_,[])	   :- true.
check_adj(X,[H])   :- isa_of(X,H).
check_adj(X,[H|T]) :- isa_of(X,H), check_adj(X,T).

% Defining pre-modifiers.
pre(T,    L, const(S0))  -->  pre1(T, L, const(S0)). % Case without possessive.
pre([G|T],L, const(S0))  -->  cn(N1, const(_)), ['s'], pre1(T,L, const(S0)),
	                             {G = ger([affiliation],N1)}.
pre1(T,   [],const(S0))  -->  pre2(T, const(S0)).
pre1([adj(A)|T],[S1|L],const(S0)) -->  adj(A,_,const(S1)), pre1(T,L,const(S0)).
pre2([],  const(_))      -->  {true}. % Potentially, no pre-modifiers.
pre2([cn([R],N1)|T], const(S0)) -->  n(N1, const(S1)), pre2(T, const(S0)),
				     {aff(cn,SS1,SS0,R), isa(S0,SS0), isa(S1,SS1)}.


% To allow compound nouns in possessives.
cn(np(N,mod([]),ext([])), const(S0)) -->  n(N, const(S0)).
cn(np(N,mod(M),ext([])),  const(S0)) -->  pre1(M, L, const(S0)), n(N, const(S0)),
	{check_adj(S0,L)}.


% Defining post-modifiers.
post(M,[],  const(S0)) --> post1(M, const(S0)). % Case without apposition.
post(M,[A], const(S0)) --> app(A, const(S0)), post1(M, const(S0)).

post1([], const(_)) --> {true}. % Potentially, no post-modifiers.

post1([rc(V,NP)],     const(S0))   -->  [that], rterm(V,trans,_, const(L0,L1)),
	                                np(NP, const(S1)),
					{reverse_rterm(V,Vs), Vs \= [isa]},
				        {isa_of(S0,L0), isa_of(S1,L1)}.
post1([pp(P,[R],NP)], const(S0))   -->  prep(P), np(NP, const(S1)),
	                                {aff(P,SS0,SS1,R), isa(S0,SS0), isa(S1,SS1)}.
post1([rc(V,NP)|M],   const(S0))   -->  [that], rterm(V,trans,_, const(L0,L1)),
	                                np(NP, const(S1)), align(_), post1(M, const(S0)),
                                        {reverse_rterm(V,Vs), Vs \= [isa],
				        isa_of(S0,L0), isa_of(S1,L1)}.
post1([rc(V,NP)|M],   const(S0))   -->  [that], rterm(V,trans,_, const(L0,L1)),
	                                np(NP, const(S1)), post1(M, const(S0)),
				        {reverse_rterm(V,Vs), Vs \= [isa],
				        isa_of(S0,L0), isa_of(S1,L1)}.
post1([pp(P,[R],NP)|M], const(S0)) -->  prep(P), np(NP,const(S1)), align(_),
	                                post1(M,const(S0)),
					{aff(P,SS0,SS1,R), isa(S0,SS0), isa(S1,SS1)}.
post1([pp(P,[R],NP)|M], const(S0)) -->  prep(P), np(NP, const(S1)), post1(M, const(S0)),
				        {aff(P,SS0,SS1,R), isa(S0,SS0), isa(S1,SS1)}.


% Defining appositions.
app(ap(N), const(_))  --> [','], [a],  np(N, const(_)), [','].
app(ap(N), const(_))  --> [','], [an], np(N, const(_)), [','].


% Defining parenthetical clauses.
app(pc(V,N), const(S0)) --> [','], [which], rterm(V,trans,_,const(L0,L1)),
	np(N,const(S1)), [','], {isa_of(S0,L0), isa_of(S1,L1)}.


% Relation terms
rterm(verb(V,mod(M)),X,T, const(S,C)) -->  rterm1(V,X,T, const(S,C)),
	       advs(P), pps(Q,V), {append(P,Q,M)}.
rterm(verb(passive(V),mod([])),X,T, const(S,C)) --> [is,V],
	       {lex(_,X,T,V,const(C,S))}, [by].
rterm(verb(passive(V),mod(M)),X,T,  const(S,C))	--> [is,V],
	       {lex(V0,X,T,V,const(C,S))}, pps(M,V0), [by].
rterm1(active(V),X,T,const(S,C)) --> [V], {lex(V, X, T, _, const(S,C))}.
% Space for prepositional verb here.


% Adverbial PPs.
pps([],_)	        -->  {true}. % Potentially no adverbial PPs.
pps([pp(P,[R],N)],V0)    -->  prep(P), np(N, const(S1)),
	{correct(V0,V), nomi(N0,V), lex(N0,noun,S0),
	 aff(P,SS0,SS1,R), isa(S0,SS0), isa(S1,SS1)}.
pps([pp(P,[R],N)|T],V0) -->  prep(P), np(N, const(S1)), align(_), pps(T, V0),
	{correct(V0,V), nomi(N0,V), lex(N0,noun,S0),
	 aff(P,SS0,SS1,R), isa(S0,SS0),isa(S1,SS1)}.
pps([pp(P,[R],N)|T],V0) -->  prep(P), np(N, const(S1)), pps(T, V0),
	{correct(V0,V), nomi(N0,V), lex(N0,noun,S0),
	 aff(P,SS0,SS1,R), isa(S0,SS0), isa(S1,SS1)}.

correct(active(V),V) :- !.
correct(passive(V),V) :- !.
correct(V,V).

% Adverbs.
advs([])		--> {true}. % Potentially no adverbs.
advs([adv(A)|T])	        --> adv(A), advs(T).

% Alignment keywords.
align(X)                -->   [X], {X = and}.
align(X)	        -->   [X], {X = ','}.

% Access to lexicon entries
prep(P)		          -->   [P], {lex(P, preposition)}.
n(n(N),  const(S0))       -->   [N], {lex(N, noun, S0)}.
adj(A,T, const(L))        -->   [A], {lex(A, adj, _, T, const(L))}.
adv(A)		          -->   [A], {lex(A, adv)}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining transitive rules for constraints.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% isa(X,Y) - returns true if X is a subclass of Y.
isa(X,X).
isa(X,Y)    :-  isa(X,Y,[]).
isa(X,Y,V)  :-  is_subset_of(X,Y), \+ memberchk((X,Y),V).
isa(X,Y,V)  :-  is_subset_of(X,Z), \+ memberchk((X,Z),V), isa(Z,Y,[(X,Y)|V]).

% isa_of(X,L) - returns true if X is a subclass of one of the elements of L.
isa_of(_,[]) :- fail.
isa_of(X,[H|T]) :- (isa(X,H) -> true ; isa_of(X,T)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

supremum(S1,S2,N) :- ( isa(S1,N), isa(S2,N) -> print_common(N,S1,S2) ; fail).

print_common(N,S1,S2) :- nl, write('Common supremum found for '), write(S1),
	                 write(' and '), write(S2), write(' with '), write(N).

reverse_rterm(verb(active(V),mod(_)), [V]).
reverse_rterm(verb(passive(V),mod(_)), [V]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%













