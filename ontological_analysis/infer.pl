%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% infer.pl:
%
%      Defining inference rules working over our facts definition.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic fact/4.
:- dynamic fact/5.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining the main predicates.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% infer_bis(+KB,-I) - returns all possible inferences I from the list
% of facts KB given in input.
%
infer(KB,I) :-
	infer_isa(KB,Z), append(KB,Z,X),
	infer_fact(X,Y), append(X,Y,W),
	filter(W,I).

% infer_isa(+KB,-I) - returns all inferences on isa relations from the
% list of facts KB given in input.
%
infer_isa(KB,I) :- setof(isa(C,D), inf_rule(KB,C,D),I), !.
infer_isa(_,[]). % In case of empty KB or no relation inferred.

% infer_fact(+KB,-I) - returns all inferences on facts from the list
% of facts KB given in input.
%
infer_fact(KB,I) :- setof(fact(T,C,V,D,R), inf_rule(KB,T,C,V,D,R),I),!.
infer_fact(_,[]). % In case of empty KB or no relation inferred.

% subclass(+KB,?C,?D) - returns true if C isa a subclass of D in KB.
%
subclass(KB,C,D)   :- subclass(KB,C,D,[]).
subclass(KB,X,Y,V) :- member(isa(X,Y),KB), \+ memberchk((X,Y),V).
subclass(KB,X,Y,V) :- member(isa(X,Z),KB), \+ memberchk((X,Z),V),
	subclass(KB,X,Y,[(X,Y)|V]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining inference rules on facts and class inclusion.
% Domain specific inference rules are not considered here.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rule 1:
% If (D isa D') & (C R D) then (C R D').
%
inf_rule(KB,T,C,V,Dsup,R) :-
	subclass(KB,D,Dsup),
	member(fact(T,C,V,D,R), KB).

% Rule 2:
% If (C' isa C) & (C R D) then (C' R D).
%
inf_rule(KB,T,Csub,V,D,R) :-
	subclass(KB,Csub,C),
	member(fact(T,C,V,D,R), KB).

% Rule 3:
% If (C' isa C)	& (D isa D') & (C R D) then (C' R D').
%
inf_rule(KB,T,Csub,V,Dsup,R) :-
	subclass(KB,Csub,C), subclass(KB,D,Dsup),
	member(fact(T,C,V,D,R), KB).

% Rule 4:
% If (C' isa C)	& (C isa D) then (C' isa D).
%
inf_rule(KB,Csub,D) :- subclass(KB,Csub,C), member(isa(C,D), KB).

% Rule 5:
% If (C isa D) & (D isa D') then (C isa D').
%
inf_rule(KB,C,Dsup) :- subclass(KB,D,Dsup), member(isa(C,D), KB).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%