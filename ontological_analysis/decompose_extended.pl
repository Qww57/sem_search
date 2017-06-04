%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Decompose.pl:
%
%         Generate a graph from natural logics proposition.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [extended_grammar].     % Loading the syntactic grammar.
:- [reverse_grammar].
:- [nominalizaton].

% :- [examples].    % Loading the predifined examples.


% decompose(S, Fs) - decompose sentence S in a list of facts Fs.
%
% Used in order to decompose a sentence into a list of facts
% whose type can be a definition, an observation, a class inclusion
% relation (isa) or a preposition relation (for instance, partonomic
% relation (in), or localisation (under)).
%
decompose(S,R) :- p(Z,S,[]), % Parse proposition
	Z = [p(TNP,TVP)], TVP = vp(V,TNP2), % Get verb and noun phrases
	nl, write(Z), nl, nl, !,
	decompose_np1(TNP,NewSNP1,T1), % Recursively decompose first NP
	decompose_np1(TNP2,NewSNP2,T2),% Recursively decompose second NP
	observation(NewSNP1,V,NewSNP2,H), % Create observation
	append(T1,T2,T), append(H,T,R),!. % Return result


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining rules to extract informations from NP.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decompose_np(TNP, NewSNP, T) :- TNP = np(n(_), mod(_), ext([])),
	decompose_np1(TNP, NewSNP, T).

% Extend to NP and not only noun !!!!!
decompose_np(TNP, NewSNP, [H|T]) :- TNP = np(n(N1), mod(M), ext([ap(N2)])),
	TNP2 = np(n(N1), mod(M), ext([])),
	H = isa([N1],[N2]), decompose_np1(TNP2, NewSNP, T).

% Active without modifiers.
decompose_np(TNP, NewSNP, [H|T]) :- TNP = np(n(N), mod(M), ext([pc(R,NP)])),
	R = verb(active(V), mod([])),
	TNP2 = np(n(N), mod(M), ext([])), decompose_np1(TNP2, NewSNP, T),
	H = fact(def,[N],[V],NewSNP).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Basic case - without any modifiers.
decompose_np1(TNP, [N], []) :- TNP = np(n(N), mod([]), ext([])).

% With only one modifier - RC.
decompose_np1(TNP, NewSNP, T) :- TNP = np(n(N), mod([rc(V,NP)]), ext([])).

% With only one modifier - PP.
decompose_np1(TNP, NewSNP, [H1,H2|T]) :- TNP = np(n(N), mod([pp(PREP,TNP2)]), ext([])),
	nl, nl, write('Decomposing as leaf PP: '), write(TNP), nl,
	decompose_np1(TNP2, NewSNP2, T), np(NewTNP2, NewSNP2, []),
	NewTNP = np(n(N), mod([pp(PREP,NewTNP2)]), ext([])), reverse_np(NewTNP, NewSNP),
	H1 = fact(prep, NewSNP, PREP, NewSNP2),	H2 = isa(NewSNP, [N]).

% With only one modifier - CN. - TODO, ADD RELATION BASED ON SEMANTIC
decompose_np1(TNP, NewSNP, H) :- TNP = np(n(N), mod([cn(n(N2))]), ext([])),
	reverse_np(TNP, NewSNP), H = isa([N],[N2]).

% With only one modifier - GER.
decompose_np1(TNP, NewSNP, [H1,H2|T]) :- TNP = np(n(N), mod([ger(R,TNP2)]), ext([])),
	nl, nl, write('Decomposing as leaf GER: '), write(TNP), nl,
	decompose_np1(TNP2, NewSNP2, T), np(NewTNP2, NewSNP2, []), write('0'),
	write(NewTNP2),
	NewTNP = np(n(N), mod([ger(R,NewTNP2)]), ext([])), !, write('1'),
	print(NewTNP),
	reverse_np(NewTNP, NewSNP), write('2'),
	H1 = fact(ger, NewSNP, R, NewSNP2), H2 = isa(NewSNP,[N]).

% With only one modifier - ADJ - intersective.
decompose_np1(TNP, NewSNP, [H1,H2]) :- TNP = np(n(N), mod([adj(A)]), ext([])),
	lex(A, adj, inter, _), reverse_np(TNP, NewSNP),
	nl, write('Decomposing as leaf ADJ: '), write(TNP), nl,
	H1 = isa(NewSNP, [N]), H2 = isa(NewSNP, [A]).

% With only one modifier - ADJ - subsective.
decompose_np1(TNP, NewSNP, [H1]) :- TNP = np(n(N), mod([adj(A)]), ext([])),
	lex(A, adj, subs, _), reverse_np(TNP, NewSNP),
	nl, write('Decomposing as leaf ADJ: '), write(TNP), nl,
	H1 = isa(NewSNP, [N]).

% With only one modifier - ADJ - non-subsective or privative
decompose_np1(TNP, NewSNP, []) :- TNP = np(n(_), mod([adj(_)]), ext([])),
	nl, write('Decomposing as leaf ADJ: '), write(TNP), nl,
	reverse_np(TNP, NewSNP).

% General case - with many modifiers.
decompose_np1(TNP,NewSNP, T) :- write('General case: '), write(TNP), nl,
	TNP = np(n(N), mod(M), ext([])), reverse_np(TNP,NewSNP),
	nsubsets(M, SubMs), !,	print(SubMs), nl,
        convert_np(N, SubMs, SubSNPs), write(SubSNPs), nl, % Convert them to string
	create_n_isa(NewSNP,SubSNPs, R1), write(R1), nl, % Create isa relations.
	decompose_subnp(SubSNPs, R2), write(R2), nl, % Decompose NPs recursively.
	append(R1,R2,T). % Append lists.

% Ending conditions.
decompose_np1(_,[],_).
decompose_np1(_,_,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining rules to create observation based on type of relation term.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Case: ISA.
observation(SNP1,isa,SNP2,[H]) :- H = isa(SNP1,SNP2).

% Case: Active verb.
observation(SNP1,verb(active(V), mod([])),SNP2,[H]) :- % Without prepositional term.
	H = fact(observation,SNP1,[V],SNP2). % Create observation.
observation(SNP1,R,SNP2,[H1,H2|T]) :- % With prepositional terms.
	R = verb(active(V), mod(M)), nomi(N,V), np(TNP1,SNP1,[]), np(TNP2,SNP2,[]),
	TrN = np(n(N), mod([pp(of,TNP2), pp(by,TNP1)|M]), ext([])), reverse_np(TrN, SrN),
	H1 = fact(observation,SNP1,[V],SNP2), H2 = attach(H1,SrN),
	decompose_np1(TrN,_,T).

% Case: Passive verb.
observation(SNP1,verb(passive(V), mod([])),SNP2,[H]) :- % Without prepositional term.
	H = fact(observation,SNP1,[is,V,by],SNP2). % Create observation.
observation(SNP1,R,SNP2,[H1,H2|T]) :- % With prepositional terms.
	R = verb(passive(V), mod(M)), lex(V1,_,_,V), nomi(N,V1),
	np(TNP1,SNP1,[]), np(TNP2,SNP2,[]),
	TrN = np(n(N), mod([pp(of,TNP2), pp(by,TNP1)|M]), ext([])), reverse_np(TrN, SrN),
	H1 = fact(observation,SNP1,[is,V,by],SNP2), H2 = attach(H1,SrN),
	decompose_np1(TrN,_,T).

% Default case.
observation(_,_,_,[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Helper functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Convert list of parse trees into strings.
convert_np(_,[],[]).
convert_np(N,[M|T1],[SNP|T2]) :- TNP = np(n(N),mod(M),ext([])),
	reverse_np(TNP,SNP), convert_np(N,T1,T2).

% Recursively decomposing a list of NPs (strings)
decompose_subnp([],[]).
decompose_subnp([HL|TL],R) :- np(TNP,HL,[]), decompose_np1(TNP,_,R1),
	decompose_subnp(TL,R2), append(R1,R2,R).

% Create n ISA relations based on a list of ISA relations: tree form
create_n_isa(_,[],[]).
create_n_isa(SNP,[H1],[H2]) :-  H2 = isa(SNP, H1).
create_n_isa(SNP,[H1|T1],[H2|T2]) :- H2 = isa(SNP, H1),
	create_n_isa(SNP,T1,T2).

% Create ISA relations from a given list of ISA relations: [isa, C].
create_isa(_,[],[]).
create_isa(N,[[isa,C]],[H2]) :- H2 = isa([N],[C]).
create_isa(N,[[isa,C]|T1],[H2|T2]) :- H2 = isa([N],[C]),
	create_isa(N,T1,T2).

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












