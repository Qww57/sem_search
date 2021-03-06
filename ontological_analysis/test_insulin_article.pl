%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% test_insulin_article.pl:
%
%	  Test algorithms on the first paragraph of wikipedia's
%	                    article on insulin.
%
%
%
% This file aims at defining test cases for Natural Logics parser,
% proposition decomposition and graph search using ontological analysis
% of concepts.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- ensure_loaded(insulin_article).   % Loading sentences to test.
:- ensure_loaded(lexicon_insulin).   % Loading the lexicon.
:- ensure_loaded(grammar).	     % Loading the grammar.
:- ensure_loaded(decompose).         % Loading decomposition algo.
:- ensure_loaded(infer).	     % Loading inference rules.
:- ensure_loaded(subsum).            % Loading subsumption rules.
:- ensure_loaded(search).	     % Loading search algorithm.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definition of functions to create the knowledge base.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_kb(Title) :-
	read_article(R),
	infer(R,R1),
	% subsum(R1,R2),
	save_as_text(R1,Title).

% save_as_text(+KB, +Title) - save the KB as a text file.
save_as_text(KB, Title) :-
    open(Title,write, Stream), write_kb(Stream,KB), close(Stream).
write_kb(_,[]).
write_kb(Stream,[H|T]) :- write(Stream,H), write(Stream,'.'), nl(Stream),
	write_kb(Stream,T).

% read_from_text(+Title, -KB) - get the KB from text file.
read_from_text(Title, KB):-
    open(Title, read, Str), read_file(Str,X), lbl(X,KB), close(Str).
read_file(Stream,[]) :- at_end_of_stream(Stream).
read_file(Stream,[X|L]) :- \+ at_end_of_stream(Stream),
    read(Stream,X), read_file(Stream,L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definition of some helper functions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decompose_sentence(X,R) :- sentence(X,Y), decompose(Y,R).
is_sentence(X) :- sentence(X, _).

read_article(R) :- setof(Y,is_sentence(Y),IDs), read_article(IDs,X), filter(X,R).
read_article([],[]).
read_article([H|T], R) :- read_article(T, R0),
	decompose_sentence(H,R1), append(R0,R1,R).

% lbl(+X,-Y) :- returns list except last element.
lbl([X|Xs], Ys) :- list_butlast_prev(Xs, Ys, X).
list_butlast_prev([], [], _).
list_butlast_prev([X1|Xs], [X0|Ys], X0) :-
   list_butlast_prev(Xs, Ys, X1).


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
test(sentence_131) :- sentence(131,X), p(_,X,[]).
test(sentence_132) :- sentence(132,X), p(_,X,[]).
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
test(sentence_241) :- sentence(241,X), p(_,X,[]).
test(sentence_250) :- sentence(250,X), p(_,X,[]).

% Decomposition of the sentences, should be successful (considering
% existing errors in the parse tree).
test(decompose_110) :- sentence(110,X), !, decompose(X,_).
test(decompose_120) :- sentence(120,X), !, decompose(X,_).
test(decompose_121) :- sentence(121,X), !, decompose(X,_).
test(decompose_122) :- sentence(122,X), !, decompose(X,_).
test(decompose_130) :- sentence(130,X), !, decompose(X,_).
test(decompose_131) :- sentence(131,X), !, decompose(X,_).
test(decompose_132) :- sentence(132,X), !, decompose(X,_).
test(decompose_140) :- sentence(140,X), !, decompose(X,_).
test(decompose_141) :- sentence(141,X), !, decompose(X,_).
test(decompose_150) :- sentence(150,X), !, decompose(X,_).
test(decompose_160) :- sentence(160,X), !, decompose(X,_).
test(decompose_170) :- sentence(170,X), !, decompose(X,_).

test(decompose_210) :- sentence(210,X), !, decompose(X,_).
test(decompose_220) :- sentence(220,X), !, decompose(X,_).
test(decompose_221) :- sentence(221,X), !, decompose(X,_).
test(decompose_230) :- sentence(230,X), !, decompose(X,_).
test(decompose_231) :- sentence(231,X), !, decompose(X,_).
test(decompose_232) :- sentence(232,X), !, decompose(X,_).
test(decompose_233) :- sentence(233,X), !, decompose(X,_).
test(decompose_240) :- sentence(240,X), !, decompose(X,_).
test(decompose_241) :- sentence(241,X), !, decompose(X,_).
test(decompose_250) :- sentence(250,X), !, decompose(X,_).

:- end_tests(insulin).

