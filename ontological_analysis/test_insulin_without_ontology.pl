%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% test_insulin_without_ontology.pl:
%
%	  Test algorithms on the first paragraph of wikipedia's
%	                    article on insulin.
%
%
%
% This file aims at defining basic test cases for Natural Logics parser.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- ensure_loaded(insulin_article).     % Loading the sentences to test.
:- ensure_loaded(extended_grammar).    % Loading the extended grammar.
:- include(insulin_lexicon).           % Loading the dedicated lexicon.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Definition of basic test cases.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(insulin_without).

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
test(sentence_241) :- sentence(241,X), p(_,X,[]).
test(sentence_250) :- sentence(250,X), p(_,X,[]).

:- end_tests(insulin_without).













