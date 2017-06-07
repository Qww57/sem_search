:- discontiguous lex/2.
:- discontiguous lex/4.
:- discontiguous nomi/2.

% Lexicon: prepositions
lex(in,   preposition).
lex(of,   preposition).
lex(by,   preposition).
lex(into, preposition).
lex(from, preposition).
lex(at,   preposition).

% Lexicon: nouns
lex(absorption,    noun).
lex(betacell,      noun).
lex(blood,         noun).
lex(cell,          noun).
lex(carbohydrate,  noun).
lex(catabolism,    noun).
lex(concentration, noun).
lex(conversion,	   noun).
lex(excretion,     noun).
lex(fat,           noun).
lex(glucose,	   noun).
lex(glycogen,	   noun).
lex(hormone,       noun).
lex(impact,        noun).
lex(insulin,       noun).
lex(islet,         noun).
lex(liver,         noun).
lex(metabolism,    noun).
lex(molecule,      noun).
lex(muscle,        noun).
lex(peptide,       noun).
lex(promotion,     noun).
lex(production,    noun).
lex(protein,       noun).
lex(regulation,    noun).
lex(synthesis,     noun).
lex(tissue,        noun).
lex(triglyceride,  noun).


% Lexicon: verbs
lex(isa,        trans, copular, _).

lex(affect,     trans, event,   affected).
lex(absorb,     trans, event,   absorbed).
lex(convert,    trans, event,   converted).
lex(excrete,    trans, event,   excreted).
lex(produce,    trans, event,   produced).
lex(promote,    trans, event,	promoted).
lex(regulate,   trans, event,   regulated).
lex(synthesize, trans, event,   synthesized).

nomi(absorption, absorb).
nomi(excretion,  excrete).
nomi(convert,    conversion).
nomi(impact,     affect).
nomi(production, produce).
nomi(promotion,  promote).
nomi(regulation, regulate).
nomi(synthesis,	 synthesize).

% Lexicon: adjectives:
lex(anabolic,    adj, inter, predi).
lex(circulating, adj, inter, predi).
lex(pancreatic,  adj, inter, predi).
lex(skeletal,    adj, inter, predi).

lex(high,        adj, subs,  predi).
lex(large,       adj, subs,  predi).
lex(low,         adj, subs,  predi).
lex(small,       adj, subs,  predi).
lex(widespread,  adj, subs,  predi).

% Lexicon: adverbs
lex(strongly, adv).

% ISA relations
isa([betacell], [cell]).
isa([alphacell], [cell]).



