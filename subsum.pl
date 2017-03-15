%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Subsum.pl:
%
%      Defining subsumption rules working over our facts definition.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [infer].

% Compute ranks of all concepts.
allRank(KB,L) :- rk(KB,L,0).
rk(KB,[],N) :- gRank(KB,[],N).
rk(KB,[H|T],N) :- gRank(KB,L,N), H = (N,L), M is N+1, rk(KB,T,M).

% Returns list of concepts L in the KB of rank R.
gRank(KB,L,0) :- findall(C,rank(KB,C,0),X),sort(X,L),!.
gRank(KB,L,R) :- R > 0, findall(C,rank(KB,C,R),X),sort(X,L),!.

% Concept C belongs to KB and is related to node of rank R.
rank(KB,C,0) :- count(KB,C,0).
rank(KB,C,1) :- member(fact(_,C,_,D),KB), count2(KB,D,0),!.
rank(KB,[],R) :- R > 1, N is R-1, gRank(KB,[],N),!.
rank(KB,C,R) :- R > 1, member(fact(_,C,_,D),KB), N is R-1,
	gRank(KB,L,N), member(D,L),!.
% The third line slows down the process for low values: for instance
% for gRank with L = 2, it was 346 inferences and now 456, but is much
% faster for higher values, such as L = 6: it was 29067 inferences
% against 3707 now.

% Returns the number N of non isa relation in the KB for concept C.
% NB: Using findall + sort instead of setof because they return an
% empty list if 0 whereas setof return false.
count(KB,C,N) :- concept(KB,C),
	findall(fact(T,C,R,D),member(fact(T,C,R,D),KB),X), sort(X,Y),
	length(Y,N).
% Count2 is used when not needed to check if C is a concept to speed up
count2(KB,C,N) :- findall(fact(T,C,R,D),member(fact(T,C,R,D),KB),X),
	sort(X,Y), length(Y,N).

% Returns the list of all concepts of the KB.
concepts(KB,C) :- setof(X,concept(KB,X),C).
concept(KB,C) :- member(fact(_,C,_,_),KB).
concept(KB,C) :- member(fact(_,_,_,C),KB).
concept(KB,C) :- member(isa(C,_),KB).
concept(KB,C) :- member(isa(_,C),KB).

% Returns the list Y of complex concepts contained in X.
% Remark, if word not defined in lexicon, considered as complex term.
complex([],[]).
complex([H1|T1],T) :- np(TNP,H1,[]),TNP = np(_,comp([])),complex(T1,T).
complex([H|T1],[H|T2]) :- complex(T1,T2).


kb(s1,[
       fact(definition,[betacell],[produce],[insulin]),
       isa([insulin],[hormone]),
       isa([betacell],[cell]),
       fact(definition,[betacell],[locatedin],[pancreas]),
       fact(definition,[body],[regulatedby],[betacell])
   ]).

kb(s210,[
       fact(observation,
	    ['{',betacell,in,organ,and,in,body,and,in,'{',pancreas,that,regulate,body,'}','}'],
	    [produce],[insulin]),
       isa(['{',betacell,in,organ,and,in,body,and,in,'{',pancreas,that,regulate,body,'}','}'],
	   ['{',betacell,in,body,and,in,'{',pancreas,that,regulate,body,'}','}']),
       isa(['{',betacell,in,organ,and,in,body,and,in,'{',pancreas,that,regulate,body,'}','}'],
	   ['{',betacell,in,organ,and,in,body,'}']),
       isa(['{',betacell,in,organ,and,in,body,and,in,'{',pancreas,that,regulate,body,'}','}'],
	   ['{',betacell,in,organ,and,in,'{',pancreas,that,regulate,body,'}','}']),
       isa(['{',betacell,in,body,and,in,'{',pancreas,that,regulate,body,'}','}'],
	   ['{',betacell,in,body,'}']),
       isa(['{',betacell,in,body,and,in,'{',pancreas,that,regulate,body,'}','}'],
	   ['{',betacell,in,'{',pancreas,that,regulate,body,'}','}']),
       fact(prep,['{',betacell,in,body,'}'],
	    in,[body]),
       isa(['{',betacell,in,body,'}'],
	   [betacell]),
       fact(prep,['{',betacell,in,'{',pancreas,that,regulate,body,'}','}'],
	    in,['{',pancreas,that,regulate,body,'}']),
       isa(['{',betacell,in,'{',pancreas,that,regulate,body,'}','}'],
	   [betacell]),
       isa(['{',pancreas,that,regulate,body,'}'],
	   [pancreas]),
       fact(definition,['{',pancreas,that,regulate,body,'}'],
	    [regulate],[body]),
       isa(['{',betacell,in,organ,and,in,body,'}'],
	   ['{',betacell,in,body,'}']),
       isa(['{',betacell,in,organ,and,in,body,'}'],
	   ['{',betacell,in,organ,'}']),
       fact(prep,['{',betacell,in,organ,'}'],
	    in,[organ]),
       isa(['{',betacell,in,organ,'}'],
	   [betacell]),
       isa(['{',betacell,in,organ,and,in,'{',pancreas,that,regulate,body,'}','}'],
	   ['{',betacell,in,organ,'}']),
       isa(['{',betacell,in,organ,and,in,'{',pancreas,that,regulate,body,'}','}'],
	   ['{',betacell,in,'{',pancreas,that,regulate,body,'}','}']),
       isa([insulin],
	   [hormone])
   ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

subsum_bis(KB,F) :- concepts(KB,C), complex(C,D),!,
	subsum_rule(KB,D,F).
subsum_bis([],[]).

subsum_rule(_,[],[]).
subsum_rule(KB,[H1|T1],T) :- np(TNP,H1,[]), TNP = np(N,comp([CC])),
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
subsum_r(KB,SNP,N,CC,[H1,H2,H3]) :-
	% Decomposing the preposition term
	CC = pp(PREP,TNP), TNP = np(N1,COMPs),
	% Finding upward subsumption rule
	subclass(KB,N1,N2), TNPnew = np(N,pp(PREP,np(N2,COMPs))),
	np(TNPnew,SNPnew,[]),
	% Creating the relations
	H1 = isa(SNP,SNPnew),
	H2 = isa(SNPnew,[N]),
	H3 = fact(prep,SNPnew,in,N2).

% Ex: from cell-in-organ -> cell-in-pancreas
subsum_r(KB,SNP,N,CC,[H1,H2,H3]) :-
	% Decomposing the preposition term
	CC = pp(PREP,TNP), TNP = np(N1,COMPs),
	% Finding downward subsumption rule
	subclass(KB,N2,N1), TNPnew = np(N,pp(PREP,np(N2,COMPs))),
	np(TNPnew,SNPnew,[]),
	% Creating the relations
	H1 = isa(SNPnew,SNP),
	H2 = isa(SNPnew,[N]),
	H3 = fact(prep,SNPnew,in,N2).

subsum_r(KB,SNP,N,CC,[H1,H2,H3]) :-
	CC = rc(vp(verb(V),np(N1,COMPs))),
	subclass(KB,[N1],[N2]), % Cas où simple / autres TODO
	% Create new SNP
	TNPnew = np(N,comp([rc(vp(verb(V),np(N2,COMPs)))])), % Rajout
	np(TNPnew,SNPnew,[]),
	% Creating the relations
	H1 = isa(SNP,SNPnew),
	H2 = isa(SNPnew,[N]),
	H3 = fact(definition,SNPnew,[V],N2).


