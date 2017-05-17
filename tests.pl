%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Tests.pl:
%		 Predicates used to generate test cases.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [decompose].      % Loading the predicate decompose.
:- [infer].	     % Loading the predicate infer.
:- [subsum].         % Loading the predicate subsum.
:- [tests_results].  % Loading the expected results of tests.


run :- findall(X,sentence(X,_),S), length(S,S1),
	write('--> Running '), write(S1), write(' tests:'), nl,
	findall([X,is,R], test(X,R),L), length(L,T1), T is S1-T1,
	display_(L), findall([X], test(X,true) ,K), length(K,J),
	write('--> '), write(T), write(' runtime errors.'), nl,
	write('--> '), write(J), write('/'), write(S1),
	write(' succeeded.'), nl, nl.

test(X,R) :- sentence(X,Z), decompose(Z,N), filter(N,M), length(M,Y),
	relations(X,Expected,_,_), equal(Expected,Y,R).

test_decompose(X,R) :-	% Loading the sentence.
	sentence(X,Z), nl, write('Sentence: '), write(Z), nl,
	decompose(Z,N), length(N,LEN), % Decomposing the sentence.
	write('---> '), write(LEN), write(' relation(s) found:'), nl,
	filter(N,M), length(M,Y), % Filtering duplicates.
	write('---> with '), write(Y), write(' distinct ones:'), nl,
	display(M), relations(X,Exp,_,_), equal(Exp,Y,R). % Display result.
test_decompose(_,R) :- test_failure(), R = false.

test_infer(X) :-
	sentence(X,S), nl, write('Sentence: '), write(S), nl,
	decompose(S,N1), filter(N1,N2), length(N2,L2), nl,
	write('--> '), write(L2), write(' relation(s) found:'), nl,
	display(N2), infer(N2,N3),length(N3,L3), nl,
	write('--> '), write(L3), write(' relation(s) infered:'), nl,
	display(N3), append(N2,N3,N4), allRank(N4,R), nl,
	write('--> '), write('Ranks of concepts: '), nl,
	display(R).
	% subsum_bis(N4,N5), length(N5,L5), nl,
	% write('--> '), write(L5), write(' rel(s) subsumed:'), nl,
	% display(N5), nl.
test_infer(_) :- test_failure().


equal(E,Y,true) :- E is Y,!.
equal(_,_,false).

filter([], []). % Filter duplicates.
filter([H|T], [H|T1]) :- subtract(T,[H],T2), filter(T2, T1).

display([]). % Display results.
display([H|T]) :- write(H), nl, display(T).

display_([]). % Display results.
display_([H|T]) :- write('--> '), write(H), nl, display_(T).

test_failure() :- nl, write('[--->] Test case failed. '), nl.

print :- listing(fact), listing(isa), listing(r_isa).
