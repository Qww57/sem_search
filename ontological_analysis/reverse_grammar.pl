%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% reverse_grammar.pl:
%
%	       Helper functions in order to convert parse trees
%                      to strings (list of words).
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded([grammar]).


% One of the challenge appears with trees that are created from
% nominalization and that might not be then the most obvious ones.
% As a consequence, the parser has problems to deal with them.
%
% Especially, this nominalization was at first giving a patient as
% object, but in some cases it can actually be a result that is then
% found in the parsing.
%
% From parsing by hand:
% --> np(n(production), mod([pp(of,[PATIENT], np(n(fat),mod([]),ext([]))),
% pp(by,[agent],np(n(pancreas),mod([]),ext([]))),
%		 pp(in,[location],np(n(liver),mod([]),ext([])))]),ext([]))
%
% From automated parsing:
% -->np(n(production), mod([pp(of,[RESULT],np(n(fat),mod([]),ext([]))),
%			    pp(by,[agent],np(n(pancreas),mod([]),ext([]))),
%			    pp(in,[location],np(n(liver),mod([]),ext([])))]),
%                      ext([])).


% reverse_np(+NP,-S) - Reverse a tree noun phrase NP into a string S
% and check that the tree structure remains unchanged.
%
% Accurate, but slow.
%
reverse_np1(NP,S) :-
	reverse_np(NP,S1), parse_np(S1,NP1),  % Parse and unparse.
	reverse_np(NP1,S), parse_np(S,NP2),   % Parse and unparse.
	NP = NP2, !. % Check that tree is unchanged.

parse_np(S,NP) :- np(NP,_,S,[]), !. % Return first guess of parser.


% reverse_np(+NP,-S) - Reverse a tree noun phrase NP into a string S
% without checking that the tree structure remains unchanged.
%
% Less accurate, but faster.
%
reverse_np(NP,S) :- % nl, nl, write('Working on: '), write(NP), nl,
	            NP = np(n(N), mod(M), ext(Ext)), lex(N, noun, _),
		    split_mod(M, Pre, Post),
		    reverse_pre(Pre, SPre), reverse_post(Post, SPost),
		    reverse_ext(Ext, SExt), append(SExt, SPost, EM),
		    append(SPre, [N], Q), append(Q, EM, S).

% Reverse pre-modifiers.
% reverse_pre(+R, -L).
reverse_pre([],[]).
reverse_pre([adj(R)|T1],[R|T2]) :- reverse_pre(T1,T2).
reverse_pre([cn(_,n(N))|T1],[N|T2]) :- reverse_pre(T1,T2).
reverse_pre([ger(_,np(n(N), mod(M), ext([])))|T1],T2) :-
	reverse_np(np(n(N),mod(M),ext([])),S), reverse_pre(T1,Q), append(S,[s|Q],T2).

% Reverse post-modifiers.
reverse_post([],[]).
reverse_post([pp(P,[_],NP)|T1],T2) :-
	alignment(T1,A), reverse_np(NP,S), append(S,A,SA),
	reverse_post(T1,Q), append([P|SA],Q,T2).
reverse_post([rc(R,NP)|T1],T2) :-
	R = verb(active(V), mod(M)),
	alignment(T1,A), reverse_np(NP,S), append(S,A,SA),
	reverse_post(M, Vmod), reverse_post(T1,Q),
	T0 = [that,V|Vmod], append(T0,SA,K),
	append(K,Q,T2).
reverse_post([rc(R,NP)|T1],T) :-
	R = verb(passive(V), mod(M)),
	alignment(T1,A), reverse_np(NP,S), append(S,A,SA),
	reverse_post(M, Vmod),
	reverse_post(T1,Q), % Reverse recursively the modifiers
	T0 = [that,is,V|Vmod], append(T0,[by],T2),
	append(T2,SA,K), append(K,Q,T).


% Alignment - defines by default an implicit alignment.
alignment(_,[]).
alignment(T1,T2) :- (T1 \= [] -> T2 = [and] ; T2 = []).

% Reverse extension.
reverse_ext([],[]).
reverse_ext([Ext],SExt) :- app(Ext, _, SExt, []).

% Split modifiers between pre-modifiers and post-modifiers.
split_mod([],[],[]).
split_mod([M|T],[M|T1],T2) :- is_pre(M), split_mod(T,T1,T2).
split_mod([M|T],T1,[M|T2]) :- is_post(M), split_mod(T,T1,T2).
is_pre(M) :- M = adj(_).
is_pre(M) :- M = cn(_,_).
is_pre(M) :- M = ger(_,_).
is_post(M) :- M = rc(_,_).
is_post(M) :- M = pp(_,_,_).

% Actually check if the relation is [isa].
reverse_rterm(verb(active(V),mod(_)), [V]).
reverse_rterm(verb(passive(V),mod(_)), [V]).








