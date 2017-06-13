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


% NOT DO PARSING UNPARSING HERE, GIVE TREE TO OBSERVATION.
% TODO - handle prepositional verb in decomposition. [NO]
% TODO - handle predicate adjectives in decomposition. [NO]
% TODO - handle ISA with modifiers. [NO]
% TODO - handle all PC cases. [NO]
% TODO - handle adjectives for nominalization. [YES]
% TODO - should deal only with trees, except at the very end when
% creating relations -> goes back to string, otherwise creating
% errors due to ambiguities by parsing unparsing again and again.

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
	decompose_np(TNP,NewTNP1,T1), !,     % Recursively decompose first NP
	decompose_np(TNP2,NewTNP2,T2), !,    % Recursively decompose second NP
	observation(NewTNP1,V,NewTNP2,H), !, % Create observation
	append(H,T1,T2,R).

decompose1(S,R) :- p(Z,S,[]), % Parse proposition
	Z = [p(TNP,TVP)], TVP = vp(V),  % Get verb and noun phrase
	% nl, write('Decomposing (special case): '), write(S), nl,
	decompose_np(TNP,NewTNP1,T), !, % Recursively decompose first NP
	observation(NewTNP1,V,H),!,     % Create observation
	append(H,T,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining rules to extract informations from NP.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decompose_np(TNP, NewTNP, T) :- TNP = np(n(_), mod(_), ext([])),
	% write('No extension: '), write(TNP), nl,
	decompose_np1(TNP, NewTNP, T).

% Apposition of a NP.
decompose_np(TNP, NewTNP, [H|T]) :- TNP = np(n(N1), mod(M), ext([ap(TNP2)])),
	% write('Extension as AP: '), write(TNP), nl,
	NTNP = np(n(N1), mod(M), ext([])), decompose_np1(TNP2, NewTNP2, T0),
	decompose_np1(NTNP, NewTNP, T1),
	reverse_np(NewTNP2, NewSNP2), H = isa([N1], NewSNP2),
        append(T0,T1,T).

% Active isa-PC without modifiers.
decompose_np(TNP, NewTNP, [H|T]) :- TNP = np(n(N), mod(M), ext([pc(R,TNP2)])),
	% nl, nl, write('Extension as PC: '), write(TNP), nl,
	R = verb(active(isa), mod([])), decompose_np(TNP2, NewTNP2, T0),
	NTNP = np(n(N), mod(M), ext([])), decompose_np(NTNP, NewTNP, T1),
	reverse_np(NewTNP, NewSNP), reverse_np(NewTNP2, NewSNP2),
	H = isa(NewSNP, NewSNP2),
	append(T0, T1, T).

% Active PC without modifiers.
decompose_np(TNP, NewTNP, [H|T]) :- TNP = np(n(N), mod(M), ext([pc(R,TNP2)])),
	% nl, nl, write('Extension as PC: '), write(TNP), nl,
	R = verb(active(V), mod([])), decompose_np(TNP2, NewTNP2, T0),
	NTNP = np(n(N), mod(M), ext([])), decompose_np(NTNP, NewTNP, T1),
	reverse_np(NewTNP, NewSNP), reverse_np(NewTNP2, NewSNP2),
	H = fact(pc, NewSNP,[V], NewSNP2, definition),
	append(T0, T1, T).

% Active PC-isa with modifiers - TODO.
% Active PC with modifiers - TODO.
% Passive PC without modifiers - TODO.
% Passsive PC with modifiers - TODO.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% NOT THE MOST EFFICIENT HERE, BUT MORE MODULAR

% VP - Active without modifiers.
% decompose_vp(+TNP1, +R, +TNP2, -NewTNP2, -T).
decompose_vp(TNP1, R, TNP2, NewTNP2, [H|T]) :- R = verb(active(V), mod([])),
	% write('Active VP without modifiers: '), write(V), nl,
	decompose_np(TNP2, NewTNP2, T),
	TNP1 = np(n(N), mod(_), ext([])),
	NewTNP1 = np(n(N), mod([rc(R, NewTNP2)]), ext([])),
	reverse_np(NewTNP1, SNP1), reverse_np(NewTNP2,SNP2),
	write(SNP1),
	H = fact(rc,SNP1,[V],SNP2,definition), !.

% VP - Active with modifiers.
decompose_vp(TNP1, R, TNP2, NewTNP2,[H1,H2|T]) :- R = verb(active(V), mod(M1)),
	% write('Active VP with modifiers: '), write(V), nl,
	nomi(N,V), nomi_adv(M1,M2), % Checking for nominalization.
	decompose_np(TNP2,NewTNP2,T1),
	TNP1 = np(n(N1), mod([_]), ext([])),
	NewTNP1 = np(n(N1), mod([]), ext([])),
	TrN = np(n(N),mod([pp(of,[patient],NewTNP2),pp(by,[agent],NewTNP1)|M2]),ext([])),
	decompose_np1(TrN,NewTrN,T2), append(T1,T2,T),
	reverse_np(NewTNP1, SNP1), reverse_np(NewTNP2, SNP2), reverse_np(NewTrN,SrN),
	write(SrN), nl,
	H1 = fact(rc,SNP1,[V],SNP2,definition), H2 = attach(H1,SrN), !.

% VP - Passive without modifiers.
decompose_vp(TNP1, R, TNP2, NewTNP2, [H|T]) :- R = verb(passive(V), mod([])),
	% write('Passive VP without modifiers: '), write(V), nl,
	decompose_np(TNP2, NewTNP2, T),
	TNP1 = np(n(N), mod(_), ext([])),
	NewTNP1 = np(n(N), mod([rc(R, NewTNP2)]), ext([])),
	reverse_np(NewTNP1,SNP1), reverse_np(NewTNP2,SNP2),
	H = fact(rc,SNP1,[is,V,by],SNP2,definition), !.

% VP - Passive with modifiers.
decompose_vp(TNP1, R, TNP2, NewTNP2, [H1,H2|T]) :- R = verb(passive(V), mod(M1)),
	% write('Passive VP with modifiers: '), write(V), nl,
	lex(V1,_,_,V,_), nomi(N,V1), nomi_adv(M1,M2), % Checking for nominalization.
	decompose_np(TNP2, NewTNP2, T1),
	TNP1 = np(n(N1), mod([_]), ext([])),
	NewTNP1 = np(n(N1), mod([]), ext([])),
	TrN = np(n(N),mod([pp(of,[patient],NewTNP2),pp(by,[agent],NewTNP1)|M2]),ext([])),
	decompose_np(TrN,NewTrN,T2), append(T1,T2,T),
	reverse_np(NewTNP1,SNP1), reverse_np(NewTNP2,SNP2), reverse_np(NewTrN,SrN),
	H1 = fact(rc,SNP1,[is,V,by],SNP2,definition), H2 = attach(H1,SrN), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Basic case - without any modifiers.
decompose_np1(TNP, NewTNP, []) :- TNP = np(n(_), mod([]), ext([])),
	% write('Basic case: '), write(TNP), nl,
	NewTNP = TNP, !.

% With only one modifier - RC.
decompose_np1(TNP, NewTNP, [H|T]) :- TNP = np(n(N), mod([rc(V,TNP2)]), ext([])),
	% nl, write('Decomposing as leaf RC:'), write(TNP), nl,
	decompose_vp(TNP, V, TNP2, NewTNP2, T),
	NewTNP = np(n(N), mod([rc(V,NewTNP2)]), ext([])),
	reverse_np(NewTNP, NewSNP),
	H = isa(NewSNP,[N]), !.

% With only one modifier - PP.
decompose_np1(TNP, NewTNP, [H1,H2|T]) :- TNP = np(n(N),mod([pp(PREP,[R],TNP2)]),ext([])),
	% nl, write('Decomposing as leaf PP: '), write(TNP), nl,
	decompose_np(TNP2, NewTNP2, T),
	NewTNP = np(n(N), mod([pp(PREP,[R],NewTNP2)]), ext([])),
	reverse_np(NewTNP, NewSNP), reverse_np(TNP2, NewSNP2),
	H1 = fact(prep, NewSNP, PREP, NewSNP2, R), H2 = isa(NewSNP, [N]), !.

% With only one modifier - CN.
decompose_np1(TNP, NewTNP, [H1,H2]) :- TNP = np(n(N), mod([cn([R],n(N2))]), ext([])),
	% nl, write('Decomposing as leaf CN: '), write(TNP), nl,
	NewTNP = TNP,
	reverse_np(TNP, NewSNP),
	H1 = isa(NewSNP,[N]), H2 = fact(cn, NewSNP, none, [N2], [R]), !.

% With only one modifier - GER.
decompose_np1(TNP, NewTNP, [H1,H2|T]) :- TNP = np(n(N), mod([ger(R,TNP2)]), ext([])),
	% nl, write('Decomposing as leaf GER: '), write(TNP), nl,
	decompose_np(TNP2, NewTNP2, T),
	NewTNP = np(n(N), mod([ger(R,NewTNP2)]), ext([])), !,
	reverse_np(NewTNP, NewSNP), reverse_np(NewTNP2, NewSNP2),
	H1 = fact(ger, NewSNP, none, NewSNP2, R), H2 = isa(NewSNP,[N]), !.

% With only one modifier - ADJ - intersective.
decompose_np1(TNP, NewTNP, [H1,H2]) :- TNP = np(n(N), mod([adj(A)]), ext([])),
	lex(A, adj, inter, _, _),
	% nl, write('Decomposing as leaf ADJ: '), write(TNP), nl,
	NewTNP = TNP,
	reverse_np(TNP, NewSNP),
	H1 = isa(NewSNP, [N]), H2 = isa(NewSNP, [A, entity]), !.

% With only one modifier - ADJ - subsective.
decompose_np1(TNP, NewTNP, [H1]) :- TNP = np(n(N), mod([adj(A)]), ext([])),
	lex(A, adj, subs, _, _),
	% nl, write('Decomposing as leaf ADJ: '), % write(TNP), nl,
	NewTNP = TNP,
	reverse_np(TNP, NewSNP),
	H1 = isa(NewSNP, [N]), !.

% With only one modifier - ADJ - non-subsective or privative
decompose_np1(TNP, NewTNP, []) :- TNP = np(n(_), mod([adj(_)]), ext([])),
	% nl, write('Decomposing as leaf ADJ: '), write(TNP), nl,
	NewTNP = TNP, !.

% General case - with many modifiers.
decompose_np1(TNP, NewTNP, T) :- TNP = np(n(N), mod(M), ext([])), M \= [],
	% nl, write('General case: '), write(TNP), nl,
	dec_modifiers(N,M,M2,R),	     % Remove isa relations recursively.
	NewTNP = np(n(N), mod(M2), ext([])), % Create the new main tree.
	nsubsets(M2, SubMs), !,              % Create new subsets of modifiers.
	convert_np(N, SubMs, SubTNPs), !,    % Convert them into proper trees.
	reverse_np(NewTNP, NewSNP),	     % Transform main tree to string.
	create_n_isa(NewSNP,SubTNPs, R1),!,  % Create isa relations.
	decompose_subnp(N, SubMs, R2),       % Decompose new trees recursively.
	append(R,R1,R2,T).                   % Append lists.

decompose_np1(X,_,[]) :- write('default case for: '), write(X), nl.

% FIX: Problem: list that is not flat, use of head here cause append crashes.
% dec_modifiers(+N,+L1,-L2,-R).
dec_modifiers(_,[],[],[]).
dec_modifiers(N, [H0|T0],[H1|T1], [R0|T2]) :- TNP = np(n(N), mod([H0]), ext([])),
	decompose_np(TNP, NewTNP, R0), NewTNP = np(n(N), mod([H1]), ext([])),
	dec_modifiers(N,T0,T1, T2), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining rules to create observation based on type of relation term.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ISA + modifiers is not handled for now properly. Only the case with
% one PP.

% Case: ISA.
observation(TNP1,verb(active(isa),mod([])),TNP2,[H]) :-
	reverse_np(TNP1,SNP1), reverse_np(TNP2,SNP2),
	H = isa(SNP1,SNP2),!.

% Case: Active verb.
observation(TNP1,verb(active(V), mod([])),TNP2,[H]) :- % Without prepositional term.
	% nl, write('Observation active without modifiers'), nl,
	reverse_np(TNP1,SNP1), reverse_np(TNP2,SNP2),
	H = fact(prop,SNP1,[V],SNP2, observation), !. % Create observation.
observation(TNP1,R,TNP2,[H1,H2|T]) :- % With prepositional terms.
	R = verb(active(V), mod(M1)), V \= isa,
	% nl, write('Observation active with modifiers'), nl, !,
	nomi(N,V), nomi_adv(M1,M2),
	TrN = np(n(N), mod([pp(of,[patient],TNP2), pp(by,[agent],TNP1)|M2]), ext([])), !,
	decompose_np1(TrN,NewTrN,T),
	reverse_np(TNP1,SNP1), reverse_np(TNP2,SNP2), reverse_np(NewTrN,SrN),
	H1 = fact(prop,SNP1,[V],SNP2, observation), H2 = attach(H1,SrN),!.

% Case: Passive verb with agent.
observation(TNP1,verb(passive(V), mod([])), TNP2,[H]) :- % Without prepositional term.
	reverse_np(TNP1,SNP1), reverse_np(TNP2,SNP2),
	H = fact(observation,SNP1,[is,V,by],SNP2), !. % Create observation.
observation(TNP1,R,TNP2,[H1,H2|T]) :- % With prepositional terms.
	R = verb(passive(V), mod(M1)),
	lex(V1,_,_,V,_), nomi(N,V1), nomi_adv(M1,M2),
	TrN = np(n(N), mod([pp(of,[result],TNP1), pp(by,[agent],TNP2)|M2]), ext([])),
	decompose_np(TrN,NewTrN,T),
	reverse_np(TNP1,SNP1), reverse_np(TNP2,SNP2), reverse_np(NewTrN,SrN),
	H1 = fact(prop,SNP1,[is,V,by],SNP2, observation), H2 = attach(H1,SrN), !.

% Default case.
observation(_,_,_,[]) :- nl, write('Default observation.'), nl.

% Case: Passive verb without agent.
observation(TNP1,verb(passive(V), mod([])),[H]) :- % Without prepositional term.
	reverse_np(TNP1,SNP1),
	H = fact(prop,SNP1,[is,V,by],[entity],observation), !. % Create observation.
observation(TNP1,R,[H1,H2|T]) :- % With prepositional terms.
	R = verb(passive(V), mod(M1)),
	lex(V1,_,_,V,_), nomi(N,V1), nomi_adv(M1,M2),
	TrN = np(n(N), mod([pp(of,[result],TNP1)|M2]), ext([])),
	decompose_np(TrN,NewTrN,T),
	reverse_np(TNP1, SNP1), reverse_np(NewTrN,SrN),
	H1 = fact(prop,SNP1,[is,V,by],[entity], observation), H2 = attach(H1,SrN), !.

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

% Convert a list of nouns(N) and modifiers(M) into a list of trees.
convert_np(_,[],[]).
convert_np(N,[M|T1],[TNP|T2]) :- TNP = np(n(N),mod(M),ext([])),
	convert_np(N,T1,T2).

% Recursively decomposing a list of NPs trees.
decompose_subnp(_,[],[]).
decompose_subnp(N,[HL|TL],R):-
	TNP = np(n(N), mod(HL), ext([])), decompose_np(TNP,_,R1), !,
	decompose_subnp(N,TL,R2), append(R1,R2,R).

% Create n ISA relations based on a list of ISA relations: tree form
create_n_isa(_,[],[]).
create_n_isa(SNP1,[H1],[H2]) :- reverse_np(H1,SNP2), H2 = isa(SNP1, SNP2).
create_n_isa(SNP1,[H1|T1],[H2|T2]) :- reverse_np(H1,SNP2),
	H2 = isa(SNP1, SNP2), create_n_isa(SNP1,T1,T2).

% Transform adverbials modifiers into concept modifiers.
nomi_adv([],[]).
nomi_adv([adv(H1)|T1],[adj(H2)|T2]) :- adv_adj(H1,H2), nomi_adv(T1,T2).
nomi_adv([H|T1],[H|T2]) :- H = pp(_,_,_), nomi_adv(T1,T2).


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

















