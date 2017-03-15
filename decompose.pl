%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Decompose.pl:
%
%         Generate a graph from natural logics proposition.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [grammar].     % Loading the syntactic grammar.
:- [examples].    % Loading the predifined examples.


% decompose(S, Fs) - decompose sentence S in a list of facts Fs.
%
% Used in order to decompose a sentence into a list of facts
% whose type can be a definition, an observation, a class inclusion
% relation (isa) or a preposition relation (for instance, partonomic
% relation (in), or localisation (under)).
%
decompose(S,R) :- proposition(Z,S,[]), % Parse proposition
	Z = s(TNP,TVP), TVP = vp(V,TNP2), % Get verb and noun phrases
	% nl, write(Z), nl, nl,
	decompose_np(TNP,NewSNP1,T1), % Recursively decompose first NP
	decompose_np(TNP2,NewSNP2,T2),% Recursively decompose second NP
	observation(NewSNP1,V,NewSNP2,H), % Create observation
	append(T1,T2,T), append(H,T,R),!. % Return result


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining rules to extract informations from NP.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% RC active pseudo leaf node without prepositions.
decompose_np(TNP,NewSNP,[H1,H2|T2]) :-
	TNP = np(N,COMPs), COMPs = comp([C]), C = rc(vp(verb(V),TNP2)),
	decompose_np(TNP2,NewSNP2,T2), % Recursively decompose terms.
	% Create ISA and definition accordingly.
	H1 = isa(NewSNP,[N]), H2 = fact(definition,NewSNP,[V],NewSNP2),
	% Create NP string without ISA relations.
	np(NewTNP2,NewSNP2,[]),
	NewTNP = np(N,comp([rc(vp(verb(V),NewTNP2))])),
	np(NewTNP, NewSNP, []).

% RC active pseudo leaf node with many preposition(s).
decompose_np(TNP,NewSNP,[H1,H2|T]) :-
	TNP = np(N,comp([C])), C = rc(vp(verb(V,pps(TPPs)),TNP2)),
	TR = verb(V,pps(TPPs)), rterm(TR,SR,[]),
	decompose_np(TNP2,NewSNP2,T1), % Recursively decompose terms.
	% Create ISA and definition accordingly.
	H1 = isa(NewSNP,[N]), H2 = fact(definition,NewSNP,SR,NewSNP2),
	% Create recursive structures
	create_n_r_isa(a,V,SR,TPPs,T2), append(T1,T2,T),
	% Create NP string without ISA relations.
	np(NewTNP2,NewSNP2,[]),
	NewTNP = np(N,comp([rc(vp(TR,NewTNP2))])),
	np(NewTNP,NewSNP,[]).

% RC - isa pseudo leaf node.
decompose_np(TNP,[N],[H|T]) :- % Name is simplified with [N].
	TNP = np(N,COMPs), COMPs = comp([C]), C = rc(vp(isa,TNP2)),
	decompose_np(TNP2,NewSNP2,T), % Recursively decompose terms.
	H = isa([N],NewSNP2). % Create ISA accordingly

% RC passive pseudo leaf node without prepositions.
decompose_np(TNP,NewSNP,[H1,H2|T2]) :-
	TNP = np(N,COMPs), COMPs = comp([C]), C = rc(vp(rpas(V),TNP2)),
	decompose_np(TNP2,NewSNP2,T2), % Recursively decompose terms.
	% Create ISA and definition accordingly.
	H1 = isa(NewSNP,[N]),
	H2 = fact(definition,NewSNP,[is,V,by],NewSNP2),
	% Create NP string without ISA relations.
	np(NewTNP2,NewSNP2,[]),
	NewTNP = np(N,comp([rc(vp(rpas(V),NewTNP2))])),
	np(NewTNP, NewSNP, []).

% RC active pseudo leaf node with preposition(s).
decompose_np(TNP,NewSNP,[H1,H2|T]) :-
	TNP = np(N,comp([C])), C = rc(vp(rpas(V,pps(TPPs)),TNP2)),
	TR = rpas(V,pps(TPPs)), rterm(TR,SR,[]),
	decompose_np(TNP2,NewSNP2,T1), % Recursively decompose terms.
	% Create ISA and definition accordingly.
	H1 = isa(NewSNP,[N]), H2 = fact(definition,NewSNP,SR,NewSNP2),
	% Create recursive structures
	create_n_r_isa(p,V,SR,TPPs,T2), append(T1,T2,T),
	% Create NP string without ISA relations.
	np(NewTNP2,NewSNP2,[]),
	NewTNP = np(N,comp([rc(vp(TR,NewTNP2))])),
	np(NewTNP,NewSNP,[]).

% PP pseudo leaf node.
decompose_np(TNP,NewSNP,[H1,H2|T]) :-
	TNP = np(N,COMPs), COMPs = comp([C]), C = pp(PREP,TNP2),
	decompose_np(TNP2, NewSNP2, T), % Recursively decompose terms.
	H1 = fact(prep,NewSNP,PREP,NewSNP2), % Create prep relation
	H2 = isa(NewSNP,[N]), % Create ISA relation
	% Create NP string without ISA relations.
	np(NewTNP2,NewSNP2,[]),NewTNP = np(N,comp([pp(PREP,NewTNP2)])),
	np(NewTNP,NewSNP,[]).

% General case.
decompose_np(TNP,NewSNP,R) :- TNP = np(N,COMPs),
	% Extract isa relations from relative clauses.
	split_rcs(COMPs,ISAs,NISAs), % Separate them
	create_isa(N,ISAs,R1), % Create the isa relations
	% Create new complements without ISA relations.
	NewTNP = np(N,comp(NISAs)), np(NewTNP,NewSNP,[]),
	% Create the n isa relations for non ISA complements.
	nsubsets(NISAs,SubNISAs), % List of sublist of size N-1
	convert_np(N, SubNISAs, SubSNPs), % Convert them to string
	create_n_isa(NewSNP,SubSNPs,R2), % Create isa relations.
	decompose_subnp(SubSNPs, R3), % Decompose NPs recursively.
	append(R1,R2,R0), append(R0,R3,R). % Append lists.

% Ending conditions.
decompose_np(_,[],_).
decompose_np(_,_,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Defining rules to create observation based on type of relation term.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Case: ISA.
observation(SNP1,isa,SNP2,[H]) :- H = isa(SNP1,SNP2).

% Case: Active verb.
observation(SNP1,verb(V),SNP2,[H]) :- % Without prepositional term.
	H = fact(observation,SNP1,[V],SNP2). % Create observation.
observation(SNP1,TR,SNP2,[H1,H2|T]) :- % With one prepositional term.
	TR = verb(R, pps([pp(_,TNP)])), rterm(TR,SR,[]), % Parse
	decompose_np(TNP,_,T), % Decompose NP recursively.
	H1 = fact(observation,SNP1,SR,SNP2), % Create observation.
	H2 = isa_r(SR,[R]). % Create ISA for the relation term.
observation(SNP1,TR,SNP2,[H|T]) :- % With many prepositional terms.
	TR = verb(R, pps(TPPs)), rterm(TR,SR,[]), % Parse.
	H = fact(observation,SNP1,SR,SNP2), % Create observation.
	create_n_r_isa(a,R,SR,TPPs,T).

% Case: Passive verb.
observation(SNP1,rpas(V),SNP2,[H]) :- % Without prespositional term.
	H = fact(observation,SNP1,[V],SNP2).
observation(SNP1,TR,SNP2,[H1,H2|T]) :- % With one prepositional term.
	rterm(TR,SR,[]), TR = rpas(V,pps([pp(_,TNP)])),
	decompose_np(TNP,_,T), % Recursively decompose NP terms.
	H1 = fact(observation,SNP1,SR,SNP2), % Create observation
	H2 = r_isa(SR,[is,V,by]). % Create iSA for the relation term.
observation(SNP1,TR,SNP2,[H|T]) :- % With many prepositional terms.
	TR = rpas(V,pps(TPPs)), rterm(TR,SR,[]),
	H = fact(observation,SNP1,SR,SNP2), % Create observation.
	create_n_r_isa(p,V,SR,TPPs,T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Helper functions dedicated to NP decomposition
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Convert list of parse trees into strings.
convert_np(_,[],[]).
convert_np(N,[COMP|T1],[SNP|T2]) :- TNP = np(N,comp(COMP)),
	np(TNP,SNP,[]), convert_np(N,T1,T2).

% Recursively decomposing a list of NPs (strings)
decompose_subnp([],[]).
decompose_subnp([HL|TL],R) :- np(TNP,HL,[]), decompose_np(TNP,_,R1),
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

% Split RCs in two lists: first one contains only ISA relations.
split_rcs(comp([]),[],[]).
split_rcs(comp([pp(P,NP)|T]), ISAs, [pp(P,NP)|NISAs]) :-
	split_rcs(comp(T), ISAs, NISAs).
split_rcs(comp([rc(H)|[]]), [H1], []) :- H = vp(isa, _), vp(H,H1,[]).
split_rcs(comp([rc(H)|T]), [H1|ISAs], NISAs) :-
	H = vp(isa, _), vp(H,H1,[]),
	split_rcs(comp(T), ISAs, NISAs).
split_rcs(comp([rc(H)|[]]), [], [H1]) :- H \= vp(isa, _), vp(H,H1,[]).
split_rcs(comp([rc(H)|T]), ISAs, [rc(H)|NISAs]) :-
	H \= vp(isa, _), split_rcs(comp(T), ISAs, NISAs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Helper functions dedicated to NP observation and its treatment of
% prepositional terms.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Create a list of rterm from V and a list of PPs.
convert_vp_active(_,[],[]). % Active case
convert_vp_active(V,[PP|T1],[SPP|T2]) :-  TR = verb(V,pps(PP)),
	rterm(TR,SPP,[]), convert_vp_active(V,T1,T2).
convert_vp_passive(_,[],[]). % Passive case
convert_vp_passive(V,[PP|T1],[SPP|T2]) :-  TR = rpas(V,pps(PP)),
	rterm(TR,SPP,[]), convert_vp_passive(V,T1,T2).

create_n_r_isa(_,_,_,[],[]).
create_n_r_isa(a,R,SR,[_],[H]) :- H = r_isa(SR,[R]).
create_n_r_isa(a,R,SR,TPPs,T):- SR \= [is|_], % Active case
	nsubsets(TPPs,SubTPPs), % Create n subsets of (n-1) prep terms.
	convert_vp_active(R,SubTPPs,SubSPPs), % Create sub rel terms.
	create_r_isa(SR,SubSPPs,T1), do_n_times(R,SubTPPs,T2),
	append(T1,T2,T).
create_n_r_isa(p,R,SR,[_],[H]) :- H = r_isa(SR,[is,R,by]).
create_n_r_isa(p,R,SR,TPPs,T):- % Passive case
	nsubsets(TPPs,SubTPPs), % Create n subsets of (n-1) prep terms.
	convert_vp_passive(R,SubTPPs,SubSPPs), % Create sub rel terms.
	create_r_isa(SR,SubSPPs,T1), do_n_times(R,SubTPPs,T2),
	append(T1,T2,T).

do_n_times(_,[],[]).
do_n_times(R,[H1|T1],M) :- TR = verb(R,pps(H1)), % Case active
	rterm(TR,SR,[]), do_n_times(R,T1,R1),
	create_n_r_isa(a,R,SR,H1,R2),
	append(R1,R2,M).
do_n_times(R,[H1|T1],M) :- TR = rpas(R,pps(H1)), % Case passive
	rterm(TR,SR,[]), do_n_times(R,T1,R1),
	create_n_r_isa(p,R,SR,H1,R2),
	append(R1,R2,M).

% Create n r_ISA relations based on a list of ISA relations: tree form
create_r_isa(_,[],[]).
create_r_isa(SR,[H1],[H2]) :-  H2 = r_isa(SR, H1).
create_r_isa(SR,[H1|T1],[H2|T2]) :- % Active case.
	H2 = r_isa(SR, H1), rterm(TR,H1,[]), TR = verb(_,pps(PPs)),
	get_np_from_pps(PPs,NPs), decompose_subnp(NPs,R1),
	create_r_isa(SR,T1,R2),	append(R1,R2,T2).
create_r_isa(SR,[H1|T1],[H2|T2]) :- % Passive case.
	H2 = r_isa(SR, H1), rterm(TR,H1,[]), TR = rpas(_,pps(PPs)),
	get_np_from_pps(PPs,NPs), decompose_subnp(NPs,R1),
	create_r_isa(SR,T1,R2),	append(R1,R2,T2).

% Extract a list of NP from a list of PPS
get_np_from_pps([],[]).
get_np_from_pps([H1|T1],[H2|T2]) :- H1 = pp(_,TNP), np(TNP,H2,[]),
	get_np_from_pps(T1,T2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% General helper functions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Partonomical rules. -- TO DELETE
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic isa/2.
% Isa is not defined as reflexive to avoid loops.
% isa(X,X).
subclass(X,Y) :- isa(X,Y).
subclass(X,Y) :- isa(X,Z), subclass(Z,Y).

:- dynamic r_isa/2.
subrelation(X,Y) :- r_isa(X,Y).
subrelation(X,Y) :- r_isa(X,Z), subrelation(Z,Y).













