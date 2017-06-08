%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Search.pl:
%
%		 Graph Search for Natural Logics KB.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- dynamic fact/4.            % Create and return facts.


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
iddfs(KB,MaxD,D,F,G,R) :- D < MaxD, dls(KB,D,F,G,Path,R),
	write('Current depth: '), write(D), nl,
	write('Nodes: '), write(Path), nl.
iddfs(KB,MaxD,D,F,G,R) :- D < MaxD, NewDeep is D+1,
	iddfs(KB,MaxD,NewDeep,F,G,R).

% dls(+KB,+D,+F,+G,-P,-R) - Depth limited DFS algorithm for the
% knowledge base KB, with a depth limit D, starting from F until the
% goal G, returning then the answer R for the path P.
%
dls(_,_,G,G,[G],[G]).
dls(KB,D,F,G,[G|Path],R) :- 0 < D, Ds is D - 1,
	dls(KB,Ds, F,OneButLast,Path,H), arc(KB,OneButLast,G,T),
	\+member(G,Path), append(H,T,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining arc for the graph search based on the knowledge base.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% arc(KB,S,G,I) - arc between states S and G contained in the
% knowledge base KB and carrying the information I.
%
% These graphs are either directly deduced from the knowledge
% (observation, definition, isa), but uses the thematic relations from
% the ontological analysis (agent, location, ...) to tailor a more
% specific answer.

% Isa relations.
arc(KB,S1,S2,I) :- member(isa(S1,S2),KB), I = [which,isa,S2].
arc(KB,S1,S2,I) :- member(isa(S2,S1),KB), I = [whereof,some,is,S2].

% Observations and definitions.
arc(KB,S1,S2,I) :- member(fact(T,S1,V,S2),KB), I = [V,by,T,S2],
	member(T,[def,observation]).
arc(KB,S1,S2,I) :- member(fact(T,S2,V,S1),KB), % Passivation of the form.
	member(T,[def,observation]),
	I = [whereof,by,T,some,are,V,d,by,S2].

% PP: in - location: cell in pancreas
% cell is located in pancreas // pancreas contains cell.
arc(KB,S1,S2,I) :- member(fact(prep,S1,in,S2),KB), I = [which,is,located,in,S2].
arc(KB,S1,S2,I) :- member(fact(prep,S2,in,S1),KB), I = [which,contain,S2].

% PP: by - manner: production by glycogenesis
% production by means of glycogenesis // glycogenesis involved in production.
arc(KB,S1,S2,I) :- member(fact(prep,S1,by,S2),KB), I = [by,means,of,S2].
arc(KB,S1,S2,I) :- member(fact(prep,S2,by,S1),KB), I = [which,is,involved,in,S2].

% PP: by - agent: production by pancreas.
% production made by pancreas // pancreas controlling production.

% PP: into - source: glucose into blood.
% glucose into blood // blood contain glucose
arc(KB,S1,S2,I) :- member(fact(prep,S1,into,S2),KB), I = [into,S2].
arc(KB,S1,S2,I) :- member(fact(prep,S2,into,S1),KB), I = [which,contain,S2].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining a small knowledge base for tests.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kb([isa([insulin],[hormone]),
    isa([hormone],[protein]),
    isa([enzyme],[protein]),
    isa([protein],[enzyme]), % just a trap to create a loop.
    fact(obs,[liver],[produce],[enzyme]),
    fact(def,[betacell],[produce],[insulin])]).


