%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% LEXICON.pl:
%			  Defining a lexicon.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic lex/2.
:- dynamic lex/3.

% Nouns
lexicon(insulin, noun).
lexicon(hormone, noun).
lexicon(pancreas, noun).
lexicon(betacell, noun).
lexicon(organ, noun).
lexicon(cell, noun).
lexicon(body, noun).

% Prepositions
lexicon(in, prep).
lexcion(under, prep).

% Verbs
lexicon(produce, verb, produced).
lexicon(regulate, verb, regulated).
