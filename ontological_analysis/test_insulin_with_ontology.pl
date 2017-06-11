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
test(sentence_110) :- sentence(110,X), p(_,X,[]).
test(sentence_120) :- sentence(120,X), p(_,X,[]).
test(sentence_121) :- sentence(121,X), p(_,X,[]).
test(sentence_122) :- sentence(122,X), p(_,X,[]).
test(sentence_130) :- sentence(130,X), p(_,X,[]).
test(sentence_140) :- sentence(140,X), p(_,X,[]).
test(sentence_141) :- sentence(141,X), p(_,X,[]).
test(sentence_150) :- sentence(150,X), p(_,X,[]).
test(sentence_160) :- sentence(160,X), p(_,X,[]).
test(sentence_171) :- sentence(170,X), p(_,X,[]).

test(sentence_210) :- sentence(210,X), p(_,X,[]).
test(sentence_220) :- sentence(220,X), p(_,X,[]).
test(sentence_221) :- sentence(221,X), p(_,X,[]).
test(sentence_230) :- sentence(230,X), p(_,X,[]).
test(sentence_231) :- sentence(231,X), p(_,X,[]).
test(sentence_232) :- sentence(232,X), p(_,X,[]).
test(sentence_233) :- sentence(233,X), p(_,X,[]).
test(sentence_240) :- sentence(240,X), p(_,X,[]).
test(sentence_250) :- sentence(250,X), p(_,X,[]).


% Decomposition of the sentences, should be successful (considering
% existing errors in the parse tree).
test(decompose_1_0) :- sentence(110,X), !, decompose(X,_).
test(decompose_2_0) :- sentence(120,X), !, decompose(X,_).
test(decompose_2_1) :- sentence(121,X), !, decompose(X,_).
test(decompose_2_2) :- sentence(122,X), !, decompose(X,_).
test(decompose_3_0) :- sentence(130,X), !, decompose(X,_).
test(decompose_4_0) :- sentence(140,X), !, decompose(X,_).
test(decompose_4_1) :- sentence(141,X), !, decompose(X,_).
test(decompose_5_0) :- sentence(150,X), !, decompose(X,_).
test(decompose_6_0) :- sentence(160,X), !, decompose(X,_).
test(decompose_7_0) :- sentence(170,X), !, decompose(X,_).

:- end_tests(insulin).

