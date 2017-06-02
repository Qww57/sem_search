:- ensure_loaded(extended_grammar).
:- ensure_loaded(reverse_grammar).

% One approach dedicated per way, in order to help the search.
% Then, second condition to help if poorly used. Assumed that parser is
% not wrong + if it is, then not ambiguous in that direction (tree ->
% string).

% 1) If only locative.
% Production in pancreas <=> produce in pancreas

% Should be theoritically the correct approach, but too inefficient +
% encountering loops.
%
% Cannot use the grammar because goes into infinite loop if used at
% such. Need to recreate some fast functions related to NP if want a
% string.

%
%  verbalize0(Noun, Rel):- np(X,Noun,[]), !, X = np(n(N), mod(M),
%  ext(_)), nomi(N, V), R = verb(active(V),mod(M)), rterm(R,_,_,Rel,[]).
% verbalize0(Noun, Rel) :- nominalize0(Noun, Rel), !.

%  nominalize0(Rel, Noun) :- rterm(R,_,_,Rel,[]), !, R = verb(active(V),
% mod(M)), nomi(N,V),  NP = np(n(N), mod(M), ext([])), np(NP, Noun, []).
% nominalize0(Rel, Noun) :- verbalize0(Rel, Noun), !.

nominalize(Rel, Noun) :- Rel = [V|T], nomi(N,V), !, Noun = [N|T].
nominalize(Rel, Noun) :- Rel = [is, Vpp| T], last(T, by), lex(V,_,_,Vpp), nomi(N,V), !,
	                 remove_last(T,T1), Noun = [N|T1].
nominalize(Rel, Noun) :- !, nominalize(Noun, Rel).

remove_last([_],[]).
remove_last([H|T1], [H|T2]) :- remove_last(T1,T2).

% 2) Of sentence.

% ?- nominalize(S, Tr, NewS) - True if the sentence S can be
% nominalized into the parse tree Tr related to the noun phrase NewS
%
% Example of use:
%
% ?- nominalize_s([cell,produce,in,pancreas,glucose],X,Z).
% X = np(n(production), mod([pp(of, np(n(glucose),mod([]),ext([]))),
%			     pp(by, np(n(cell),mod([]),ext([]))),
%			     pp(in, np(n(pancreas),mod([]),ext([])))]),
%                       ext([])),
% Z = [production, of, glucose, by, cell, in, pancreas].
%
nominalize_s(S, Tr, NewS) :- p(P,S,[]), !, write(P), P = [p(NP1, VP)], !,
	VP = vp(verb(active(V), mod(M)),NP2), nomi(N0,V),
	NewM = [pp(of,NP2),pp(by,NP1)|M], Tr = np(n(N0), mod(NewM), ext([])),
	reverse_np(Tr, NewS).

nomi(production, produce).
nomi(synthesis, synthetize).




