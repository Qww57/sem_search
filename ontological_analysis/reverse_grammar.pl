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


% TODO EXTENSION.
% TODO, take care of 'AND'
%  np(X,Z,[liver,release,of,glucagon,in,blood,by,glycogenolysis,and,by,gluconeogenesis])
%
% could be understood differently without the 'and'.
% 243 ?- np(X,Z,[liver,release,of,glucagon,in,blood,by,glycogenolysis,and,by,
% gluconeogenesis],[]), reverse_np(X,Y), np(M,Z,Y,[]).
% X = np(n(release), mod([cn([agent], n(liver)), pp(of, [patient],
% np(n(glucagon), mod([]), ext([]))), pp(in, [location], np(n(blood),
% mod([]), ext([]))), pp(by, [manner], np(n(glycogenolysis), mod([]),
% ext([]))), pp(by, [manner], np(n(gluconeogenesis), mod([]),
% ext([])))]), ext([])),
% Z = const(process),
% Y = [liver, release, of, glucagon, in, blood, by, glycogenolysis, by,
% gluconeogenesis],
% M = np(n(release), mod([cn([agent], n(liver)), pp(of, [patient],
% np(n(glucagon), mod([]), ext([]))), pp(in, [location], np(n(blood),
% mod([]), ext([]))), pp(by, [manner], np(n(glycogenolysis), mod([pp(by,
% [manner], np(n(gluconeogenesis), mod([]), ext([])))]), ext([])))]),
% ext([])) .

% Reverse a noun phrase.
reverse_np(NP,S) :- % nl, nl, write('Working on: '), write(NP), nl,
	            NP = np(n(N), mod(M), ext(Ext)), lex(N, noun, _), % write(0),
		    split_mod(M, Pre, Post), % write(1),
		    reverse_pre(Pre, SPre), reverse_post(Post, SPost), % write(2),
		    reverse_ext(Ext, SExt), append(SExt, SPost, EM), % write(3),
		    append(SPre, [N], Q), append(Q, EM, S). % EM instead

% Reverse pre-modifiers.
reverse_pre([],[]).
reverse_pre([adj(R)|T1],[R|T2]) :- reverse_pre(T1,T2).
reverse_pre([cn(_,n(N))|T1],[N|T2]) :- reverse_pre(T1,T2).
reverse_pre([ger(_,np(n(N), mod(M), ext([])))|T1],T2) :-
	reverse_np(np(n(N),mod(M),ext([])),S), reverse_pre(T1,Q), append(S,[s|Q],T2).

% Reverse pro-modifiers.
reverse_post([],[]).
reverse_post([pp(P,[_],NP)|T1],T2) :- reverse_np(NP,S),
	reverse_post(T1,Q), append([P|S],Q,T2) .
reverse_post([rc(R,NP)|T1],T2) :- R = verb(active(V), mod(M)), reverse_np(NP,S),
	reverse_post(M, Vmod), reverse_post(T1,Q), T0 = [that,V|Vmod], append(T0, S, K),
	append(K,Q,T2).
reverse_post([rc(R,NP)|T1],T) :- R = verb(passive(V), mod(M)),
	reverse_np(NP,S), % Reverse NP in the RC.
	reverse_post(M, Vmod), reverse_post(T1,Q), !, % Reverse recursively the modifiers
	T0 = [that,is,V|Vmod], append(T0,[by],T2), append(T2, S, K), append(K,Q,T).

% Reverse extension.
reverse_ext([],[]).
reverse_ext([Ext],SExt) :- app(Ext, _, SExt, []).

% Split modifiers between pre-modifiers and post-modifiers.
split_mod([],[],[]).
split_mod([M|T],[M|T1],T2) :- is_pre(M), split_mod(T,T1,T2).
split_mod([M|T],T1,[M|T2]) :- is_post(M), split_mod(T,T1,T2).
is_post(M) :- M = rc(_,_).
is_post(M) :- M = pp(_,_,_).
is_pre(M) :- M = adj(_).
is_pre(M) :- M = cn(_,_).
is_pre(M) :- M = ger(_,_).

% Actually check if the relation is [isa].
reverse_rterm(verb(active(V),mod(_)), [V]).
reverse_rterm(verb(passive(V),mod(_)), [V]).


