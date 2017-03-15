%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Infer.pl:
%
%      Defining inference rules working over our facts definition.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [grammar].        % Get rterm for inferences on relations.
:- [decompose].      % Get decomposition methods for relations.
:- dynamic fact/4.   % Create and return facts.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining the main predicates.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% infer_bis(KB,I) - finds all possible inferences F from the list
% of facts KB given in input.
%
infer(KB,I) :- setof(fact(T,C,R,D), inf_rule(KB,T,C,R,D),I),!.
infer(_,[]). % In case of empty KB or no relation inferred.

subclass(KB,C,D) :- member(isa(C,D),KB).
subclass(KB,C,D) :- member(isa(C,Z),KB), subclass(KB,Z,D).

subrel(KB,C,D) :- member(r_isa(C,D),KB).
subrel(KB,C,D) :- member(r_isa(C,Z),KB), subrel(KB,Z,D).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining basic inference rules.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Rule 1: If (D isa D') & (C R D) then (C R D').
%
inf_rule(KB,T,C,R,Dsup) :- subclass(KB,D,Dsup),
	member(fact(T,C,R,D), KB).

% Rule 2: If (C' isa C) & (C R D) then (C' R D).
%
inf_rule(KB,T,Csub,R,D) :- subclass(KB,Csub,C),
	member(fact(T,C,R,D), KB).

% Rule 3: If (C' isa C)	& (D isa D') & (C R D) then (C' R D').
%
inf_rule(KB,T,Csub,R,Dsup) :- subclass(KB,Csub,C), subclass(KB,D,Dsup),
	member(fact(T,C,R,D), KB).

% Rule 4: If (R isa R') & (C R D) then (C R' D').
%
inf_rule(KB,T,C,Rsup,D) :- subrel(KB,R,Rsup),
	member(fact(T,C,R,D), KB).

% Rule 5: If (C [R-in-B] D) & (B isa B') then (C [R-in-B'] D').
%
% inf_rule(KB,T,C,Rsup,D) :- infer_verb(KB,R,Rsup),
%	member(fact(T,C,R,D), KB).


% Basic case - TODO extend to infer from different pps.
%
infer_verb(KB,R,Rsup) :- % Active relation.
	rterm(TR,R,[]), TR = verb(V,pps([pp(PREP,TC)])),
	np(TC,C,[]), subclass(KB,C,Csup), np(TCsup,Csup,[]),
	TRnew = verb(V,pps([pp(PREP,TCsup)])), rterm(TRnew,Rsup,[]).

infer_verb(KB,R,Rsup) :- % Passive relation.
	rterm(TR,R,[]), TR = rpas(V,pps([pp(PREP,TC)])),
	np(TC,C,[]), subclass(KB,C,Csup), np(TCsup,Csup,[]),
	TRnew = rpas(V, pps([pp(PREP,TCsup)])), rterm(TRnew,Rsup,[]).













