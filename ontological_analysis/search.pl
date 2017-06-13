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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining arc for the graph search based on the knowledge base.
%
% Arc with ontology.
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

% Observations and definitions.
arc(KB,S1,S2,I) :-
	member(fact(_,S1,V,S2,T),KB),
	member(T,[def,observation]),
	I = [which,V,by,T,S2].
arc(KB,S1,S2,I) :-
	member(fact(_,S2,V,S1,T),KB), % Passivation of the form.
	member(T,[def,observation]),
	I = [whereof,by,T,some,are,V,d,by,S2].

% PP: in - location: cell in pancreas
arc(KB,S1,S2,I) :-
	member(fact(prep,S1,in,S2,_),KB),
	I = [which,is,located,in,S2].

% PP: by - manner: production by glycogenesis
arc(KB,S1,S2,I) :-
	member(fact(prep,S1,by,S2,_),KB),
	I = [by,means,of,S2].

% PP: by - agent: production by pancreas.

% PP: into - source: glucose into blood.
arc(KB,S1,S2,I) :-
	member(fact(prep,S1,into,S2,_),KB),
	I = [into,S2].

% PP: into - direction: glucose into blood.
% glucose into blood // blood contain glucose
arc(KB,S1,S2,I) :- member(fact(prep,S1,of,S2,_),KB), I = [of,S2].

% PP: from - origin: glucose from blood.
arc(KB,S1,S2,I) :- member(fact(prep,S1,from,S2,_),KB), I = [from,S2].


arc(KB,S1,S2,I) :- member(fact(cn,S1,_,S2,R),KB), I = [which,is,related,by,R,to,S2].
arc(KB,S1,S2,I) :- member(fact(ger,S1,_,S2,R),KB), I = [which,is,related,by,R,to,S2].



% PP - Inverse relation
arc(KB,S1,S2,I) :- member(isa(S2,S1),KB), I = [whereof,some,is,S2].
arc(KB,S1,S2,I) :- member(fact(prep,S2,by,S1,_),KB), I = [which,is,involved,in,S2].
arc(KB,S1,S2,I) :- member(fact(prep,S2,from,S1,_),KB), I = [which,is,the,source,of,S2].
arc(KB,S1,S2,I) :- member(fact(prep,S2,of,S1,_),KB), I = [which,is,related,to,S2].
arc(KB,S1,S2,I) :- member(fact(prep,S2,in,S1,_),KB), I = [which,contain,S2].
arc(KB,S1,S2,I) :- member(fact(prep,S2,into,S1,_),KB), I = [which,contain,S2].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining a small knowledge base for tests.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

kb1(s0, [
       fact(prop,[betacell],[produce],[insulin],definition),
       isa([insulin],[hormone]),
       isa([betacell],[cell]),
       fact(prop,[betacell],[located_in],[pancreas],definition),
       fact(prop,[body],[is,regulated,by],[betacell],definition),
       fact(prop,[metabolism],[is,hosted,by],[body], definition)
   ]).


kb1(s1, [isa([insulin],[hormone]),
    isa([hormone],[protein]),
    isa([enzyme],[protein]),
    isa([protein],[enzyme]), % just a trap to create a loop.
    isa([cell,that,produce,insulin],[betacell]),
    fact(prop,[liver],[produce],[enzyme],observation),
    fact(prop,[betacell],[produce],[insulin],definition),
    fact(prop,[betacell,that,produce,insulin], [produce], [insulin], definition)]).


