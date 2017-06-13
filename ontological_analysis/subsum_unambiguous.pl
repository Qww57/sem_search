%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Subsum.pl:
%
%      Defining subsumption rules working over our facts definition.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded([unambiguous_grammar]).
:- ensure_loaded([reverse_unambiguous]).
:- ensure_loaded([infer_unambiguous]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Computing rank of concepts.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Compute ranks of all concepts.
allRank(KB,L) :- rk(KB,[],[],0,L).

% rk(+KB,+AlreadySeen,+PrevRanks,-CurrentRank,-Rank).
rk(KB,[],[],0,Acc) :- gRank(KB,[],L,[],0), L \= [], H = (0,L),
	M is 1, rk(KB,L,[H],M,Acc), !.
rk(KB,S,T,N,Acc) :- N > 0, gRank(KB,S,L,T,N), L \= [], H = (N,L), M is N+1,
	append(S,L,Seen),rk(KB,Seen,[H|T],M,Acc), !.
rk(KB,_,T,N,Acc) :- N > 0, gRank(KB,_,L,T,N), L == [], T = Acc.

% Returns list of concepts L in the KB of rank R.
gRank(KB,Seen,L,Prev,R) :- my_setof(C,rank(KB,Seen,C,Prev,R),L), !.

% rank(+KB,+C,-R) - Returns rank R of concept C in KB.
rank(KB,[],C,[],0) :- count(KB,C,_,0). % write(C), write(' is ranked 0'), nl.
rank(KB,Seen,C,Prev,R) :- R > 0, member(fact(_,C,_,D,_),KB),
	\+ member(C,Seen), N is R-1, concepts_of_rank(Prev,N,L), member(D,L).
	% write(C), write(' is ranked '), write(R), nl.

concepts_of_rank([H|T],R,L) :- (H = (R,L) -> true; concepts_of_rank(T,R,L)), !.
concepts_of_rank(_,_,[]).

% count(+KB,-C,-N) - Returns the number N of non isa relation in the KB
% for concept C. NB: Using findall + sort instead of setof because they
% return an empty list if 0 whereas setof return false.
%
count(KB,C,Z,N) :- concept(KB,C),
	my_setof(fact(T,C,R,D,V),member(fact(T,C,R,D,V),KB),Z),
	length(Z,N).

% concepts(+KB,-C) - Returns the list of all concepts defined in the KB.
%
concepts(KB,C) :- setof(X,concept(KB,X),C).
concept(KB,C)  :- member(fact(_,C,_,_,_),KB).
concept(KB,C)  :- member(fact(_,_,_,C,_),KB).
concept(KB,C)  :- member(isa(C,_),KB).
concept(KB,C)  :- member(isa(_,C),KB).

% my_setof(Obj, Goal, List) - Returns the set of all
my_setof(Obj, Goal, List) :- (\+ Goal -> List = [] ; setof(Obj,Goal,List)).

test_rank_zero(_,[]).
test_rank_zero(KB,[H|T]) :- count(KB,H,_,0), !, test_rank(KB,T), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Returns the list Y of complex concepts contained in X.
% Remark, if word not defined in lexicon, considered as complex term.
complex([],[]).
complex([H1|T1],T)     :- np(TNP,H1,[]),TNP = np(_,comp([])),complex(T1,T).
complex([H|T1],[H|T2]) :- complex(T1,T2).


subsum_bis(KB,F) :- concepts(KB,C), complex(C,D),!,
	subsum_rule(KB,D,F).
subsum_bis([],[]).

subsum_rule(_,[],[]).
subsum_rule(KB,[H1|T1],T) :- np(TNP,_,H1,[]), TNP = np(N, mod([CC]), ext([])),
	setof([R1,R2,R3],subsum_r(KB,H1,N,CC,[R1,R2,R3]),L1),
	subsum_rule(KB,T1,L2), append(L1,L2,T).
subsum_rule(_,_,[]).

% KB  - knowledge base: list of facts.
% SNP - name of the complex noun
% N   -	noun of the complex noun
% CC  - complements of the complex noun
% H1  - isa relation between SNP and new concept S
% H2  - isa relation between S and N
% H3  - fact relation between S and N2
%
% Ex: from cell-in-pancreas -> cell-in-organ
subsum_r(KB,TNP,[H1,H2,H3]) :-
	% Decomposing the preposition term
	TNP = np(n(N), mod(CC), ext([])), CC = [pp(PREP,_,TNP2)],
	% Finding upward subsumption rule
	TNP2 = np(n(N1), mod(M), ext([])), subclass(KB,N1,N2),
	NewTNP2 = np(n(N2), mod(M), ext([])),
	TNPnew = np(n(N),pp(PREP,_,NewTNP2)),
	% Creating the relations
	reverse_np(TNPnew,SNPnew), reverse_np(TNP,SNP),
	H1 = isa(SNP,SNPnew),
	H2 = isa(SNPnew,[N]),
	H3 = fact(prep,SNPnew,in,N2).

subsum_r(KB,SNP,N,CC,[H1,H2,H3]) :-
	CC = rc(vp(verb(V),np(N1,COMPs))),
	subclass(KB,[N1],[N2]),
	% Create new SNP
	TNPnew = np(N,comp([rc(vp(verb(V),np(N2,COMPs)))])), % Rajout
	reverse_np(TNPnew,SNPnew),
	% Creating the relations
	H1 = isa(SNP,SNPnew),
	H2 = isa(SNPnew,[N]),
	H3 = fact(definition,SNPnew,[V],N2).













