:- discontiguous lex/2.
:- discontiguous lex/4.
:- discontiguous nomi/2.

% Lexicon: prepositions
lex(in, preposition).
lex(of, preposition).
lex(by, preposition).

% Lexicon: determiners.
lex(some, preposition).

% Lexicon: nouns
lex(doctor, noun).
lex(table, noun).
lex(disease, noun).
lex(production, noun).
lex(glucogenesis, noun).
lex(conversion, noun).
lex(glucose, noun).
lex(insulin, noun).
lex(alphacell, noun).
lex(betacell, noun).
lex(cell, noun).
lex(pancreas, noun).
lex(concentration, noun).
lex(behaviour, noun).

% Lexicon: verbs
% Distinction between 'effect, copular and state'
lex(smile, intrans, effect).
lex(isa, trans, copular, _).
lex(remain, trans, copular, _).
lex(observe, trans, effect, observed).
lex(produce, trans, effect, produced).
lex(reside, trans, state, hosted).

% Lexicon: adjectives
lex(red, adj, subs, predi).
lex(young, adj, subs, predi).
lex(synchronous, adj, inter, predi).
lex(possible, adj, non_subs, non_predi).
lex(former, adj, privative, non_predi).

% Lexicon: adverbs
lex(synchronously, adv).

% ISA relations
isa([betacell], [cell]).
isa([alphacell], [cell]).



