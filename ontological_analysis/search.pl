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


% search(+KB,+D,+S,+G,-A) - predicate querying the knowledge base KB
% using IDDFS with a maximum depth D, with S as start state and G
% as goal state in order to return the answer A.
%
% Example:
% ?- query(KB,[insulin],[protein],R).
% R = [insulin, which, isa, hormone, which, isa, protein].
%
search(KB,D,First,Goal,R) :- iddfs(KB,D,0,First,Goal,R).

iddfs(KB,MaxD,D,F,G,R) :-
	D < MaxD, dls(KB,D,F,G,Path,R),
	write('Current depth: '), write(D), nl,
	write('Nodes: '), write(Path), nl.
iddfs(KB,MaxD,D,F,G,R) :-
	D < MaxD, NewDeep is D+1,
	iddfs(KB,MaxD,NewDeep,F,G,R).

% dls(+KB,+D,+F,+G,-P,-R) - Depth limited DFS algorithm for the
% knowledge base KB, with a depth limit D, starting from F until the
% goal G, returning then the answer R for the path P.
%
dls(_,_,G,G,[G],[G]).
dls(KB,D,F,G,[G|Path],R) :-
	0 < D, Ds is D - 1,
	dls(KB,Ds, F,OneButLast,Path,H), arc(KB,OneButLast,G,T),
	\+member(G,Path), append(H,T,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining direct arcs for the graph search.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% arc(+KB,+S,+G,-I) - arc between states S and G contained in the
% knowledge base KB and carrying the information I.
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
arc(KB,S1,S2,I) :- % Isa relations.
	member(isa(S1,S2),KB),
	I = [which,isa,S2].


arc(KB,S1,S2,I) :- % Observations and definitions.
	member(fact(_,S1,V,S2,T),KB),
	member(T,[def,observation]),
	I = [which,V,by,T,S2].
arc(KB,S1,S2,I) :-
	member(fact(_,S2,V,S1,T),KB), % Passivation of the form.
	member(T,[def,observation]),
	I = [whereof,by,T,some,are,V,d,by,S2].


arc(KB,S1,S2,I) :- % PP: in - location: cell in pancreas.
	member(fact(prep,S1,in,S2,location),KB),
	I = [which,is,located,in,S2].
arc(KB,S1,S2,I) :- % PP: in - affiliation TODO
	member(fact(prep,S1,in,S2,affiliation),KB),
	I = [which,is,related,to,S2].
arc(KB,S1,S2,I) :- % PP: in - manner TODO
	member(fact(prep,S1,in,S2,manner),KB),
	I = [which,is,todo,S2].


arc(KB,S1,S2,I) :- % PP: by - agent: production by pancreas.
	member(fact(prep,S1,by,S2,agent),KB),
	I = [made,by,S2].
arc(KB,S1,S2,I) :- % PP: by - instrument: payment by credit card.
	member(fact(prep,S1,by,S2,instrument),KB),
	I = [made,by,S2].
arc(KB,S1,S2,I) :- % PP: by - manner: production by glycogenesis.
	member(fact(prep,S1,by,S2,manner),KB),
	I = [by,means,of,S2].


arc(KB,S1,S2,I) :- % PP: of - result: concentration of glucose.
	member(fact(prep,S1,of,S2,beared_by),KB),
	I = [which,is,beared,by,S2].
arc(KB,S1,S2,I) :- % PP: of - result: synthesis of production.
	member(fact(prep,S1,of,S2,has_part),KB),
	I = [which,has,part,S2].
arc(KB,S1,S2,I) :- % PP: of - result: synthesis of production.
	member(fact(prep,S1,of,S2,part_of),KB),
	I = [which,is,part,of,S2].
arc(KB,S1,S2,I) :- % PP: of - patient: conversion of glucose.
	member(fact(prep,S1,of,S2,patient),KB),
	I = [which,involves,S2].
arc(KB,S1,S2,I) :- % PP: of - result: production of glucose.
	member(fact(prep,S1,of,S2,result),KB),
	I = [which,creates,S2].


arc(KB,S1,S2,I) :- % PP: into - source: glucose into blood.
	member(fact(prep,S1,into,S2,direction),KB),
	I = [in,the,direction,of,S2].
arc(KB,S1,S2,I) :- % PP: into - result: transformation into glucose.
	member(fact(prep,S1,into,S2,result),KB),
	I = [which,creates,S2].


arc(KB,S1,S2,I) :- % PP: from - origin: glucose from blood.
	member(fact(prep,S1,from,S2,origin),KB),
	I = [that,come,from,S2].


arc(KB,S1,S2,I) :- % CN: default
	member(fact(cn,S1,none,S2,R),KB),
	I = [which,is,related,by,R,to,S2].


arc(KB,S1,S2,I) :- % GER: default
	member(fact(ger,S1,none,S2,R),KB),
	I = [which,is,related,by,R,to,S2].



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining indirect arcs for the graph search.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


arc(KB,S1,S2,I) :- % ISA relation.
	member(isa(S2,S1),KB),
	I = [whereof,some,is,S2].

arc(KB,S1,S2,I) :- % Observation and definition.
	member(fact(prep,S2,by,S1,_),KB),
	I = [which,is,involved,in,S2].


arc(KB,S1,S2,I) :- % PP: in - location: cell in pancreas.
	member(fact(prep,S2,in,S1,location),KB),
	I = [which,contains,S2].
arc(KB,S1,S2,I) :- % PP: in - affiliation TODO
	member(fact(prep,S2,in,S1,affiliation),KB),
	I = [which,is,related,to,S2].
arc(KB,S1,S2,I) :- % PP: in - manner TODO
	member(fact(prep,S2,in,S1,manner),KB),
	I = [which,is,todo,S2].


arc(KB,S1,S2,I) :- % PP: of - result: concentration of glucose.
	member(fact(prep,S2,of,S1,beared_by),KB),
	I = [which,is,the,bearer,of,S2].
arc(KB,S1,S2,I) :- % PP: of - result: synthesis of production.
	member(fact(prep,S2,of,S1,has_part),KB),
	I = [which,is,part,of,S2].
arc(KB,S1,S2,I) :- % PP: of - result: synthesis of production.
	member(fact(prep,S2,of,S1,part_of),KB),
	I = [which,has,part,S2].
arc(KB,S1,S2,I) :- % PP: of - patient: conversion of glucose.
	member(fact(prep,S2,of,S1,patient),KB),
	I = [which,is,involved,in,S2].
arc(KB,S1,S2,I) :- % PP: of - result: production of glucose.
	member(fact(prep,S2,of,S1,result),KB),
	I = [which,is,created,by,S2].


arc(KB,S1,S2,I) :- % PP: into - source: glucose into blood.
	member(fact(prep,S2,into,S1,direction),KB),
	I = [which,contains,S2].
arc(KB,S1,S2,I) :- % PP: into - result: transformation into glucose.
	member(fact(prep,S2,into,S1,result),KB),
	I = [which,originates,from,S2].


arc(KB,S1,S2,I) :- % PP: from - source: glucose from blood.s
	member(fact(prep,S2,from,S1,source),KB),
	I = [which,is,the,source,of,S2].


arc(KB,S1,S2,I) :- % CN: default
	member(fact(cn,S2,none,S1,R),KB),
	I = [which,is,related,by,R,to,S2].


arc(KB,S1,S2,I) :- % GER: default
	member(fact(ger,S2,none,S1,R),KB),
	I = [which,is,related,by,R,to,S2].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
