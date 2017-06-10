%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% test_insulin_with_ontology.pl:
%
%	  Test algorithms on the first paragraph of wikipedia's
%	                    article on insulin.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- ensure_loaded(insulin_article).           % Loading sentences to test.
:- ensure_loaded(insulin_onthology_lexicon). % Loading the lexicon.
:- ensure_loaded(unambiguous_grammar).	     % Loading the grammar.
:- ensure_loaded(decompose_unambiguous).     % Loading decomposition algo.
:- ensure_loaded(search).	             % Loading search algorithm.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definition of some helper functions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decompose_sentence(X,R) :- sentence(X,Y), decompose(Y,R).
is_sentence(X) :- sentence(X, _).

read_article(R) :- setof(Y,is_sentence(Y),IDs), read_article(IDs,X), filter(X,R).
read_article([],[]).
read_article([H|T], R) :- read_article(T, R0), decompose_sentence(H,R1), append(R0,R1,R).

write_kb(_,[]).
write_kb(Stream,[H|T]) :- write(Stream, '-> '), write(Stream,H), nl(Stream),
	write_kb(Stream,T).

save_as_txt(KB, Title) :-
    open('first_paragraph.txt',write, Stream), length(KB,L),
    write(Stream, '-------------------- '),
    write(Stream, Title), write(Stream, ' --------------------'),
    nl(Stream), write(Stream, L), write(Stream, ' relations'),
    nl(Stream), nl(Stream), write_kb(Stream,KB),
    close(Stream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definition of basic test cases.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(insulin).

% Parsing of the sentences, should be successful.
test(sentence_1_0) :- sentence(10,X), p(_,X,[]).
test(sentence_2_0) :- sentence(20,X), p(_,X,[]).
test(sentence_2_1) :- sentence(21,X), p(_,X,[]).
test(sentence_2_2) :- sentence(22,X), p(_,X,[]).
test(sentence_3_0) :- sentence(30,X), p(_,X,[]).
test(sentence_4_0) :- sentence(40,X), p(_,X,[]).
test(sentence_4_1) :- sentence(41,X), p(_,X,[]).
test(sentence_5_0) :- sentence(50,X), p(_,X,[]).
test(sentence_6_0) :- sentence(60,X), p(_,X,[]).
test(sentence_7_0) :- sentence(70,X), p(_,X,[]).

% Decomposition of the sentences, should be successful (considering
% existing errors in the parse tree).
test(decompose_1_0) :- sentence(10,X), !, decompose(X,_).
test(decompose_2_0) :- sentence(20,X), !, decompose(X,_).
test(decompose_2_1) :- sentence(21,X), !, decompose(X,_).
test(decompose_2_2) :- sentence(22,X), !, decompose(X,_).
test(decompose_3_0) :- sentence(30,X), !, decompose(X,_).
test(decompose_4_0) :- sentence(40,X), !, decompose(X,_).
test(decompose_4_1) :- sentence(41,X), !, decompose(X,_).
test(decompose_5_0) :- sentence(50,X), !, decompose(X,_).
test(decompose_6_0) :- sentence(60,X), !, decompose(X,_).
test(decompose_7_0) :- sentence(70,X), !, decompose(X,_).

:- end_tests(insulin).

