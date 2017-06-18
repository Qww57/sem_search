%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Search.pl:
%
%		 Graph Search for Natural Logics KB.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic fact/5.            % Create and return facts.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% IDDFS (iterative deepening depth first search).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% search(+KB,+D,+S,-A) - predicate querying the knowledge base KB
% using IDDFS with a maximum depth D, with S as start state and G
% as goal state in order to return the answer A.
%
% This algorithm tries frist to find paths without reverted edges.
%
% Example:
% ?- search(KB,4,[insulin],R).
% R =[[insulin], which, isa,
%     [peptide, hormone, that, is, produced, by, betacell, of,
%    pancreatic, islet]] R = [insulin, which, which, isa, protein].
%
search(KB,D,First,[every|R]) :-
	iddfs(KB,D,0,First,_,R,false), R \= [First].
search(KB,D,First,[some|R]) :-
	iddfs(KB,D,0,First,_,R,true),  R \= [First].


% search(+KB,+D,+S,+G,-A) - predicate querying the knowledge base KB
% using IDDFS with a maximum depth D, with S as start state and G
% as goal state in order to return the answer A.
%
% This algorithm tries frist to find paths without reverted edges.
%
% Example:
% ?- search(KB,4,[insulin],[protein],R).
% R = [insulin, which, isa, hormone, which, isa, protein].
%
search(KB,D,First,Goal,[every|R]) :-
	iddfs(KB,D,0,First,Goal,R,false).
search(KB,D,First,Goal,[some|R]) :-
	iddfs(KB,D,0,First,Goal,R,true).
search(_,_,F,G,R) :-
	R = [F, and, G, are, apparently, not, connected].


iddfs(KB,MaxD,D,F,G,R,A) :-
	D < MaxD, dls(KB,D,F,G,Path,R,A),
	write('Current depth: '), write(D), nl,
	write('Nodes: '), write(Path), nl.
iddfs(KB,MaxD,D,F,G,R,A) :-
	D < MaxD, NewDeep is D+1,
	iddfs(KB,MaxD,NewDeep,F,G,R,A).

% dls(+KB,+D,+F,+G,-P,-R,-A) - Depth limited DFS algorithm for the
% knowledge base KB, with a depth limit D, starting from F until the
% goal G, returning then the answer R for the path P. The parameter A,
% indicates if reversed arcs are allowed.
%
dls(_,_,G,G,[G],[G],_).
dls(KB,D,F,G,[G|Path],R,A) :-
	0 < D, Ds is D - 1,
	dls(KB,Ds, F,OneButLast,Path,H,A),
	arc(KB,OneButLast,G,[I1,I2],A),
	\+member(G,Path),
	(Path = [F] -> append(H,I1,R) ; append(H,I2,R)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining direct arcs for the graph search.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% arc(+KB,+S,+G,-I,-R) - arc between states S and G contained in the
% knowledge base KB and carrying the information I. R indicates if the
% arc is reverted or not.
%
% These graphs are either directly deduced from the knowledge
% (observation, definition, isa), but uses the thematic relations from
% the ontological analysis (agent, location, ...) to tailor a more
% specific answer.
%
% PPs involving 'in_response_to', 'at' and 'as' are not represented here
% since they only define comparison, conditions and cause which are not
% handled by the graph search.
%
arc(KB,S1,S2,[I1,I2],_) :- % Isa relations.
	member(isa(S1,S2),KB),
	I1 = [is,a,S2],
	I2 = [which,is,a,S2].


arc(KB,S1,S2,[I1,I2],_) :- % Observations and definitions.
	member(fact(_,S1,V,S2,T),KB),
	member(T,[def,observation]),
	I1 = [V, by, T, S2],
	I2 = [which, V, by, T, S2].


arc(KB,S1,S2,[I1,I2],_) :- % PP: in - location: cell in pancreas.
	member(fact(prep,S1,in,S2,location),KB),
	I1 = [is,located,in,S2],
	I2 = [which,is,located,in,S2].
arc(KB,S1,S2,[I1,I2],false) :- % PP: in - affiliation TODO
	member(fact(prep,S1,in,S2,affiliation),KB),
	I1 = [is, related, to, S2],
	I2 = [which,is,related,to,S2].
arc(KB,S1,S2,I,false) :- % PP: in - manner TODO
	member(fact(prep,S1,in,S2,manner),KB),
	I = [which,is,todo,S2].


arc(KB,S1,S2,[I1,I2],_) :- % PP: by - agent: production by pancreas.
	member(fact(prep,S1,by,S2,agent),KB),
	I1 = [made, by, S2],
	I2 = [which,is,made,by,S2].
arc(KB,S1,S2,[I1,I2],_) :- % PP: by - instrument: payment by credit card.
	member(fact(prep,S1,by,S2,instrument),KB),
	I1 = [is,made,using,S2],
	I2 = [which,is,made,using,S2].
arc(KB,S1,S2,[I1,I2],_) :- % PP: by - manner: production by glycogenesis.
	member(fact(prep,S1,by,S2,manner),KB),
	I1 = [is,made,by,means,of,S2],
	I2 = [which,is,made,by,means,of,S2].


arc(KB,S1,S2,[I1,I2],_) :- % PP: of - result: concentration of glucose.
	member(fact(prep,S1,of,S2,beared_by),KB),
	I1 = [is,beared,by,S2],
	I2 = [which,is,beared,by,S2].
arc(KB,S1,S2,[I1,I2],_) :- % PP: of - result: synthesis of production.
	member(fact(prep,S1,of,S2,has_part),KB),
	I1 = [has,part,S2],
	I2 = [which,has,part,S2].
arc(KB,S1,S2,[I1,I2],_) :- % PP: of - result: synthesis of production.
	member(fact(prep,S1,of,S2,part_of),KB),
	I1 = [is,part,of,S2],
	I2 = [which,is,part,of,S2].
arc(KB,S1,S2,[I1,I2],_) :- % PP: of - patient: conversion of glucose.
	member(fact(prep,S1,of,S2,patient),KB),
	I1 = [involves,S1],
	I2 = [which,involves,S2].
arc(KB,S1,S2,[I1,I2],_) :- % PP: of - result: production of glucose.
	member(fact(prep,S1,of,S2,result),KB),
	I1 = [creates,S2],
	I2 = [which,creates,S2].


% TODO FROM HERE.

arc(KB,S1,S2,[I1,I2],_) :- % PP: into - source: production into blood.
	member(fact(prep,S1,into,S2,direction),KB),
	I1 = [is,made,in,the,direction,of,S2],
	I2 = [which,is,made,in,the,direction,of,S2].
arc(KB,S1,S2,[I1,I2],_) :- % PP: into - result: transformation into glucose.
	member(fact(prep,S1,into,S2,result),KB),
	I1 = [creates, S2],
	I2 = [which,creates,S2].


arc(KB,S1,S2,[I1,I2],_) :- % PP: from - origin: glucose from blood.
	member(fact(prep,S1,from,S2,origin),KB),
	I1 = [comes,from,S2],
	I2 = [that,comes,from,S2].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

arc(KB,S1,S2,[I1,I2],_) :- % CN: agent
	member(fact(cn,S1,none,S2,agent),KB),
	I1 = [initiates,S2],
	I2 = [which,initiates,S2].

arc(KB,S1,S2,[I1,I2],_) :- % CN: bearer
	member(fact(cn,S1,none,S2,bearer),KB),
	I1 = [bears,S2],
	I2 = [which,bears,S2].

arc(KB,S1,S2,[I1,I2],_) :- % CN: agent
	member(fact(cn,S1,none,S2,location),KB),
	I1 = [is,located,in,S2],
	I2 = [which,is,located,in,S2].

arc(KB,S1,S2,[I1,I2],_) :- % CN: patient
	member(fact(cn,S1,none,S2,patient),KB),
	I1 = [is,affected,by,S2],
	I2 = [which,is,affected,by,S2].

arc(KB,S1,S2,[I1,I2],_) :- % CN: result
	member(fact(cn,S1,none,S2,result),KB),
	I1 = [results,from,S2],
	I2 = [which,results,from,S2].

arc(KB,S1,S2,[I1,I2],_) :- % CN: default
	member(fact(cn,S1,none,S2,R),KB),
	I1 = [is,related,by,R,to,S2],
	I2 = [which,is,related,by,R,to,S2].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% TO DETAIL
arc(KB,S1,S2,[I1,I2],_) :- % GER: default
	member(fact(ger,S1,none,S2,R),KB),
	I1 = [is,related,by,R,to,S2],
	I2 = [which,is,related,by,R,to,S2].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining indirect arcs for the graph search.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


arc(KB,S1,S2,[I1,I2],true) :- % ISA relation.
	member(isa(S2,S1),KB),
	I1 = [are, a, S2],
	I2 = [whereof,some,are,a,S2].

arc(KB,S1,S2,[I1,I2],true) :-
	member(fact(_,S2,V,S1,T),KB), % Passivation of the form.
	member(T,[def,observation]),
	I1 = [V, by, T, S2],
	I2 = [whereof,by,T,some,are,V,d,by,S2].

arc(KB,S1,S2,[I1,I2],true) :- % PP: in - location: cell in pancreas.
	member(fact(prep,S2,in,S1,location),KB),
	I1 = [is,the,host,of,S2],
	I2 = [which,is,the,host,of,S2].
arc(KB,S1,S2,[I1,I2],true) :- % PP: in - affiliation TODO
	member(fact(prep,S2,in,S1,affiliation),KB),
	I1 = [which,is,related,to,S2],
	I2 = [which,is, related,to,S2].
arc(KB,S1,S2,[I1,I2],true) :- % PP: in - manner TODO
	member(fact(prep,S2,in,S1,manner),KB),
	I1 = [is,todo,S2],
	I2 = [which,is,todo,S2].

arc(KB,S1,S2,[I1,I2],true) :- % PP: of - result: concentration of glucose.
	member(fact(prep,S2,of,S1,beared_by),KB),
	I1 = [bears, S2],
	I2 = [which,bears,S2].
arc(KB,S1,S2,[I1,I2],true) :- % PP: of - result: synthesis of production.
	member(fact(prep,S2,of,S1,has_part),KB),
	I1 = [is,part,of,s2],
	I2 = [which,is,part,of,S2].
arc(KB,S1,S2,[I1,I2],true) :- % PP: of - result: synthesis of production.
	member(fact(prep,S2,of,S1,part_of),KB),
	I1 = [has,part,S2],
	I2 = [which,has,part,S2].
arc(KB,S1,S2,[I1,I2],true) :- % PP: of - patient: conversion of glucose.
	member(fact(prep,S2,of,S1,patient),KB),
	I1 = [which,is,affected,in,S2],
	I2 = [which,is,affected,in,S2].
arc(KB,S1,S2,[I1,I2],true) :- % PP: of - result: production of glucose.
	member(fact(prep,S2,of,S1,result),KB),
	I1 = [is,created,by,S2],
	I2 = [which,is,created,by,S2].


arc(KB,S1,S2,[I1,I2],true) :- % PP: into - source: glucose into blood.
	member(fact(prep,S2,into,S1,direction),KB),
	I1 = [is,the,destination,of,S2],
	I2 = [which,is,the,destination,of,S2].
arc(KB,S1,S2,[I1,I2],true) :- % PP: into - result: transformation into glucose.
	member(fact(prep,S2,into,S1,result),KB),
	I1 = [originates,from,S2],
	I2 = [which,originates,from,S2].


arc(KB,S1,S2,[I1,I2],true) :- % PP: from - source: glucose from blood.
	member(fact(prep,S2,from,S1,source),KB),
	I1 = [is,the,source,of,S2],
	I2 = [which,is,the,source,of,S2].


arc(KB,S1,S2,[I1,I2],true) :- % CN: default.
	member(fact(cn,S2,none,S1,R),KB),
	I1 = [is,related,by,R,to,S2],
	I2 = [which,is,related,by,R,to,S2].


arc(KB,S1,S2,[I1,I2],true) :- % GER: default.
	member(fact(ger,S2,none,S1,R),KB),
	I1 = [is,related,by,R,to,S2],
	I2 = [which,is,related,by,R,to,S2].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%











