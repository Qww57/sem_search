%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Decompose_unambiguous.pl:
%
%         Generate a graph from natural logics proposition.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Since trees are different, add extra information in the knowledge.

:- [unambiguous_grammar].		 % Loading the syntactic grammar.
:- ensure_loaded([reverse_unambiguous]). % Loading helper functions to unparse a tree.

% TODO - handle prepositional verb in decomposition.
% TODO - handle predicate adjectives in decomposition.
% TODO - handle ISA with modifiers.
% TODO - handle all PC cases.
% TODO - handle adjectives for nominalization.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining rules to extract information from propositions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% decompose(S, Fs) - decompose sentence S in a list of facts Fs.
%
% Used in order to decompose a sentence into a list of facts
% whose type can be a definition, an observation, a class inclusion
% relation (isa) or a preposition relation (for instance, partonomic
% relation (in), or localisation (under)).
%
decompose(S,R) :- decompose1(S,X), flatten(X,Y), filter(Y,R), nl,
	length(R,M), write(M), write(' relations found.'), nl, nl.

decompose1(S,R) :- p(Z,S,[]), % Parse proposition
	Z = [p(TNP,TVP)], TVP = vp(V,TNP2),  % Get verb and noun phrases
	% nl, write('Decomposing: '), write(S),
	decompose_np(TNP,NewSNP1,T1), !,     % Recursively decompose first NP
	decompose_np(TNP2,NewSNP2,T2), !,    % Recursively decompose second NP
	observation(NewSNP1,V,NewSNP2,H), !, % Create observation
	append(H,T1,T2,R).

decompose1(S,R) :- p(Z,S,[]), % Parse proposition
	Z = [p(TNP,TVP)], TVP = vp(V),  % Get verb and noun phrase
	% nl, write('Decomposing (special case): '), write(S), nl,
	decompose_np(TNP,NewSNP1,T), !, % Recursively decompose first NP
	observation(NewSNP1,V,H),!,     % Create observation
	append(H,T,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining rules to extract informations from NP.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decompose_np(TNP, NewSNP, T) :- TNP = np(n(_), mod(_), ext([])),
	% write('No extension: '), write(TNP), nl,
	decompose_np1(TNP, NewSNP, T).

% Apposition of a NP.
decompose_np(TNP, NewSNP, [H|T]) :- TNP = np(n(N1), mod(M), ext([ap(TNP2)])),
	% write('Extension as AP: '), write(TNP), nl,
	NewTNP = np(n(N1), mod(M), ext([])), decompose_np1(TNP2, NewSNP2, T0),
	H = isa([N1], NewSNP2), decompose_np(NewTNP, NewSNP, T1),
	append(T0,T1,T).

% Active isa-PC without modifiers.
decompose_np(TNP, NewSNP, [H|T]) :- TNP = np(n(N), mod(M), ext([pc(R,TNP2)])),
	% nl, nl, write('Extension as PC: '), write(TNP), nl,
	R = verb(active(isa), mod([])), decompose_np(TNP2, NewSNP2, T0),
	TNPbis = np(n(N), mod(M), ext([])), decompose_np(TNPbis, NewSNP, T1),
	H = isa(NewSNP, NewSNP2), append(T0, T1, T).

% Active PC without modifiers.
decompose_np(TNP, NewSNP, [H|T]) :- TNP = np(n(N), mod(M), ext([pc(R,TNP2)])),
	% nl, nl, write('Extension as PC: '), write(TNP), nl,
	R = verb(active(V), mod([])), decompose_np(TNP2, NewSNP2, T0),
	TNPbis = np(n(N), mod(M), ext([])), decompose_np(TNPbis, NewSNP, T1),
	H = fact(def, NewSNP,[V], NewSNP2), append(T0, T1, T).

% Active PC-isa with modifiers - TODO.
% Active PC with modifiers - TODO.
% Passive PC without modifiers - TODO.
% Passsive PC with modifiers - TODO.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% VP - Active without modifiers.
decompose_vp(SNP1, R, NP, [H|T]) :- R = verb(active(V), mod([])),
	H = fact(def,SNP1,[V],SNP2), decompose_np(NP, SNP2, T).

% VP - Passive with modifiers.
decompose_vp(SNP1, R, TNP2, [H1,H2|T]) :- R = verb(active(V), mod(M)),
	nomi(N,V), % Checking for nominalization.
	np(TNP1,_,SNP1,[]), reverse_np(TNP2,SNP2), % Get the tree structures of NP.
	TrN = np(n(N), mod([pp(of,[patient],TNP2), pp(by,[agent],TNP1)|M]), ext([])),
	H1 = fact(def,SNP1,[V],SNP2), H2 = attach(H1,SrN),
	decompose_np(TrN,SrN,T).

% VP - Passive without modifiers.
decompose_vp(SNP1, R, NP, [H|T]) :- R = verb(passive(V), mod([])),
	H = fact(def,SNP1,[is,V,by],SNP2), decompose_np(NP, SNP2, T).

% VP - Passive with modifiers.
decompose_vp(SNP1, R, TNP2, [H1,H2|T]) :- R = verb(passive(V), mod(M)),
	lex(V1,_,_,V,_), nomi(N,V1), % Checking for nominalization.
	np(TNP1,_,SNP1,[]), reverse_np(TNP2,SNP2), % Get the tree structures of NP.
	TrN = np(n(N), mod([pp(of,[patient],TNP2), pp(by,[agent],TNP1)|M]), ext([])),
	H1 = fact(def,SNP1,[is,V,by],SNP2), H2 = attach(H1,SrN),
	decompose_np(TrN,SrN,T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Basic case - without any modifiers.
decompose_np1(TNP, [N], []) :- TNP = np(n(N), mod([]), ext([])).
	% write('Basic case: '), write(TNP), nl.

% With only one modifier - RC.
decompose_np1(TNP, NewSNP, [H|T]) :- TNP = np(n(N), mod([rc(V,NP)]), ext([])),
	% nl, write('Decomposing as leaf RC:'), write(TNP), nl,
	reverse_np(TNP, NewSNP), H = isa(NewSNP,[N]), decompose_vp(NewSNP, V, NP, T), !.

% With only one modifier - PP.
decompose_np1(TNP, NewSNP, [H1,H2|T]) :- TNP = np(n(N),mod([pp(PREP,[R],TNP2)]),ext([])),
	% nl, write('Decomposing as leaf PP: '), write(TNP), nl,
	decompose_np(TNP2, NewSNP2, T), np(NewTNP2, _, NewSNP2, []),
	NewTNP = np(n(N), mod([pp(PREP,[R],NewTNP2)]), ext([])),
	reverse_np(NewTNP, NewSNP),
	H1 = fact(prep, NewSNP, PREP, NewSNP2, R), H2 = isa(NewSNP, [N]), !.

% With only one modifier - CN.
decompose_np1(TNP, NewSNP, [H1,H2]) :- TNP = np(n(N), mod([cn([R],n(N2))]), ext([])),
	% nl, write('Decomposing as leaf CN: '), write(TNP), nl,
	reverse_np(TNP, NewSNP),
	H1 = isa(NewSNP,[N]),
	H2 = fact(ger, NewSNP, [R], [N2]), !.

% With only one modifier - GER.
decompose_np1(TNP, NewSNP, [H1,H2|T]) :- TNP = np(n(N), mod([ger(R,TNP2)]), ext([])),
	% nl, write('Decomposing as leaf GER: '), write(TNP), nl,
	decompose_np(TNP2, NewSNP2, T), np(NewTNP2, _,NewSNP2, []),
	NewTNP = np(n(N), mod([ger(R,NewTNP2)]), ext([])), !,
	reverse_np(NewTNP, NewSNP),
	H1 = fact(ger, NewSNP, R, NewSNP2), H2 = isa(NewSNP,[N]), !.

% With only one modifier - ADJ - intersective.
decompose_np1(TNP, NewSNP, [H1,H2]) :- TNP = np(n(N), mod([adj(A)]), ext([])),
	lex(A, adj, inter, _, _), reverse_np(TNP, NewSNP),
	% nl, write('Decomposing as leaf ADJ: '), write(TNP), nl,
	H1 = isa(NewSNP, [N]), H2 = isa(NewSNP, [A, entity]), !.

% With only one modifier - ADJ - subsective.
decompose_np1(TNP, NewSNP, [H1]) :- TNP = np(n(N), mod([adj(A)]), ext([])),
	lex(A, adj, subs, _, _), reverse_np(TNP, NewSNP),
	% nl, write('Decomposing as leaf ADJ: '), % write(TNP), nl,
	H1 = isa(NewSNP, [N]), !.

% With only one modifier - ADJ - non-subsective or privative
decompose_np1(TNP, NewSNP, []) :- TNP = np(n(_), mod([adj(_)]), ext([])),
	% nl, write('Decomposing as leaf ADJ: '), write(TNP), nl,
	reverse_np(TNP, NewSNP), !.

% General case - with many modifiers.
decompose_np1(TNP,NewSNP, T) :- TNP = np(n(N), mod(M), ext([])), M \= [],
	% nl, write('General case: '), write(TNP), nl,
	dec_modifiers(N,M,M2,R),	     % Remove isa relations recursively.
	NewTNP = np(n(N), mod(M2), ext([])), % Create the new main tree.
	reverse_np(NewTNP, NewSNP),	     % Transform main tree to string.
	nsubsets(M2, SubMs), !,		     % Create new subsets of modifiers.
	convert_np(N, SubMs, SubSNPs), !,    % Convert them into string.
	create_n_isa(NewSNP,SubSNPs, R1),!,  % Create isa relations.
	decompose_subnp(N, SubMs, R2),       % Decompose new trees recursively.
	append(R,R1,R2,T).                   % Append lists.

decompose_np1(X,_,[]) :- write('default case for: '), write(X), nl.

% FIX: Problem: list that is not flat, use of head here cause append crashes.
dec_modifiers(_,[],[],[]).
dec_modifiers(N, [H0|T0],[H1|T1], [R0|T2]) :- TNP = np(n(N), mod([H0]), ext([])),
	decompose_np(TNP, NewSNP, R0), np(NewTNP, _, NewSNP, []),
	NewTNP = np(n(N), mod([H1]), ext([])), !, dec_modifiers(N,T0,T1, T2), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining rules to create observation based on type of relation term.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ISA + modifiers is not handled for now properly. Only the case with
% one PP.

% Case: ISA.
observation(SNP1,verb(active(isa),mod([])),SNP2,[H]) :- H = isa(SNP1,SNP2),!.

% Case: Active verb.
observation(SNP1,verb(active(V), mod([])),SNP2,[H]) :- % Without prepositional term.
	% nl, write('Observation active without modifiers'), nl,
	H = fact(observation,SNP1,[V],SNP2), !. % Create observation.
observation(SNP1,R,SNP2,[H1,H2|T]) :- % With prepositional terms.
	R = verb(active(V), mod(M)), V \= isa, !,
	% nl, write('Observation active with modifiers'), nl, !,
	nomi(N,V), np(TNP1,_,SNP1,[]), np(TNP2,_,SNP2,[]),
	TrN = np(n(N), mod([pp(of,[patient],TNP2), pp(by,[agent],TNP1)|M]), ext([])), !,
	H1 = fact(observation,SNP1,[V],SNP2), H2 = attach(H1,SrN),
	decompose_np1(TrN,SrN,T),!.

% Case: Passive verb with agent.
observation(SNP1,verb(passive(V), mod([])),SNP2,[H]) :- % Without prepositional term.
	H = fact(observation,SNP1,[is,V,by],SNP2), !. % Create observation.
observation(SNP1,R,SNP2,[H1,H2|T]) :- % With prepositional terms.
	R = verb(passive(V), mod(M)), lex(V1,_,_,V,_), nomi(N,V1),
	np(TNP1,_,SNP1,[]), np(TNP2,_,SNP2,[]),
	TrN = np(n(N), mod([pp(of,[result],TNP1), pp(by,[agent],TNP2)|M]), ext([])),
	H1 = fact(observation,SNP1,[is,V,by],SNP2), H2 = attach(H1,SrN),
	decompose_np(TrN,SrN,T), !.

% Default case.
observation(_,_,_,[]) :- nl, write('Default observation.'), nl.

% Case: Passive verb without agent.
observation(SNP1,verb(passive(V), mod([])),[H]) :- % Without prepositional term.
	H = fact(observation,SNP1,[is,V,by],[entity]), !. % Create observation.
observation(SNP1,R,[H1,H2|T]) :- % With prepositional terms.
	R = verb(passive(V), mod(M)), lex(V1,_,_,V,_), nomi(N,V1),
	np(TNP1,_,SNP1,[]), TrN = np(n(N), mod([pp(of,[result],TNP1)|M]), ext([])),
	H1 = fact(observation,SNP1,[is,V,by],[entity]),
	decompose_np(TrN,SrN,T),  H2 = attach(H1,SrN), !.

% Default case.
observation(_,_,[]) :- nl, write('Default observation.'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Helper functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Append three lists together.
append([],[],L,L).
append([],[H|T],L,[H|R]) :- append([],T,L,R).
append([H|T],L0,L1,[H|R]) :- append(T,L0,L1,R).

% Filter duplicates in a list.
filter([], []).
filter([H|T], [H|T1]) :- subtract(T,[H],T2), filter(T2, T1).

% Convert list of parse trees into strings.
convert_np(_,[],[]).
convert_np(N,[M|T1],[SNP|T2]) :- TNP = np(n(N),mod(M),ext([])),
	reverse_np(TNP,SNP), convert_np(N,T1,T2).

% Recursively decomposing a list of NPs trees.
decompose_subnp(_,[],[]).
decompose_subnp(N,[HL|TL],R):-
	TNP = np(n(N), mod(HL), ext([])), decompose_np(TNP,_,R1), !,
	decompose_subnp(N,TL,R2), append(R1,R2,R).

% Create n ISA relations based on a list of ISA relations: tree form
create_n_isa(_,[],[]).
create_n_isa(SNP,[H1],[H2]) :-  H2 = isa(SNP, H1).
create_n_isa(SNP,[H1|T1],[H2|T2]) :- H2 = isa(SNP, H1),
	create_n_isa(SNP,T1,T2).

% Determine the n subsets of length n-1 of a list of size n.
%
% Example of use:
% ?- nsubsets([a,b,c],X).
% X = [[a, b], [a, c], [b, c]].
%
nsubsets([],[]).
nsubsets(X,R) :- length(X,L), N is L-1, setof(Y,subset(N,X,Y),R).
subset(0,[],[]).
subset(L,[E|T1],[E|T2]):- succ(PL,L),(PL>0->subset(PL,T1,T2); T2=[]).
subset(L, [_|T1], T2):- subset(L,T1,T2).

















