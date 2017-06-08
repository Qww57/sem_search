:- ensure_loaded(extended_grammar).

reverse_np(NP,S) :- % nl, nl, write('Working on: '), write(NP), nl,
	            NP = np(n(N), mod(M), ext(Ext)), lex(N, noun), % write(Ext),
	            % write('Modifiers: '), write(M), !, nl,
		    split_mod(M, Pre, Post),
		    % write('Pre: '), write(Pre), nl,
		    % write('Post: '), write(Post), nl,
		    reverse_pre(Pre, SPre), reverse_post(Post, SPost), !,
		    reverse_ext(Ext, SExt), append(SExt, SPost, EM),
		    % write(SExt),
		    % write('Pre string: '), write(SPre), nl,
	            % write('Post string: '), write(EM), nl,
		    append(SPre, [N], Q), append(Q, EM, S).

reverse_pre([],[]).
reverse_pre([adj(R)|T1],[R|T2]) :- reverse_pre(T1,T2).
reverse_pre([cn(n(N))|T1],[N|T2]) :- reverse_pre(T1,T2).
reverse_pre([ger(_,np(n(N), mod(M), ext([])))|T1],T2) :-
	reverse_np(np(n(N),mod(M),ext([])),S), reverse_pre(T1,Q), append(S,[s|Q],T2).

reverse_post([],[]).
reverse_post([pp(P,NP)|T1],T2) :- reverse_np(NP,S), reverse_post(T1,Q), append([P|S],Q,T2) .
reverse_post([rc(R,NP)|T1],T2) :- R = verb(active(V), mod(M)), reverse_np(NP,S),
	reverse_post(M, Vmod), reverse_post(T1,Q), T0 = [that,V|Vmod], append(T0, S, K),
	append(K,Q,T2).
reverse_post([rc(R,NP)|T1],T) :- R = verb(passive(V), mod(M)),
	reverse_np(NP,S), % Reverse NP in the RC.
	reverse_post(M, Vmod), reverse_post(T1,Q), !, % Reverse recursively the modifiers
	T0 = [that,is,V|Vmod], append(T0,[by],T2), append(T2, S, K), append(K,Q,T).

reverse_ext([],[]).
reverse_ext([Ext],SExt) :- app(Ext, SExt, []).

split_mod([],[],[]).
split_mod([M|T],[M|T1],T2) :- is_pre(M), split_mod(T,T1,T2).
split_mod([M|T],T1,[M|T2]) :- is_post(M), split_mod(T,T1,T2).

is_post(M) :- M = rc(_,_).
is_post(M) :- M = pp(_,_).
is_pre(M) :- M = adj(_).
is_pre(M) :- M = cn(_).
is_pre(M) :- M = ger(_,_).

% Actually check if the relation is [isa].
reverse_rterm(verb(active(V),mod(_)), [V]).
reverse_rterm(verb(passive(V),mod(_)), [V]).

