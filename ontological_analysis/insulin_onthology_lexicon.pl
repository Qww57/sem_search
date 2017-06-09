%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Insulin_lexicon.pl:
%
%	       Dedicated lexicon for the first paragraph of
%		   the wikipedia article on insulin.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- discontiguous lex/2.
:- discontiguous lex/4.
:- discontiguous lex/5.
:- discontiguous nomi/2.


% Lexicon: prepositions
lex(in,   preposition).
lex(of,   preposition).
lex(by,   preposition).
lex(into, preposition).
lex(from, preposition).
lex(at,   preposition).


% Lexicon: nouns
lex(absorption,    noun, process).
lex(betacell,      noun, cell).
lex(blood,         noun, body_fluid).
lex(cell,          noun, cell).
lex(carbohydrate,  noun, body_fluid).
lex(catabolism,    noun, process).
lex(concentration, noun, quantity).
lex(conversion,	   noun, transformation).
lex(excretion,     noun, creation).
lex(fat,           noun, body_fluid).
lex(glucose,	   noun, body_fluid).
lex(glycogen,	   noun, body_fluid).
lex(hormone,       noun, protein).
lex(impact,        noun, enhancement).
lex(insulin,       noun, protein).
lex(islet,         noun, body_part).
lex(liver,         noun, organ).
lex(metabolism,    noun, process).
lex(molecule,      noun, mass).
lex(muscle,        noun, body_part).
lex(pancreas,      noun, organ).
lex(peptide,       noun, protein).
lex(promotion,     noun, enhancement).
lex(production,    noun, creation).
lex(protein,       noun, protein).
lex(regulation,    noun, enhancement).
lex(synthesis,     noun, creation).
lex(tissue,        noun, body_part).
lex(triglyceride,  noun, body_fluid).


% Lexicon: verbs
lex(isa,        trans, copular, const(X,X),                        is_some).

lex(affect,     trans, event,   const(_,_),                        affected).
lex(absorb,     trans, event,   const(process, body_fluid),        absorbed).
lex(absorb,     trans, event,   const(body_part, body_fluid),      absorbed).
lex(convert,    trans, event,   const(transformation, body_fluid), converted).
lex(convert,    trans, event,   const(body_part, body_fluid),      converted).
lex(excrete,    trans, event,   const(creative, body_fluid),       excreted).
lex(excrete,    trans, event,   const(body_part, body_fluid),      excreted).
lex(inhibit,    trans, event,   const(process, process),           inhibited).
lex(produce,    trans, event,   const(creation, body_fluid),       produced).
lex(produce,    trans, event,   const(body_part, body_fluid),      produced).
lex(promote,    trans, event,   const(process, process),	   promoted).
lex(regulate,   trans, event,   const(process, process),           regulated).
lex(synthesize, trans, event,   const(process, state),             synthesized).
lex(synthesize, trans, event,   const(process, body_fluid),        synthesized).

nomi(absorption, absorb).
nomi(excretion,  excrete).
nomi(conversion, convert).
nomi(impact,     affect).
nomi(production, produce).
nomi(promotion,  promote).
nomi(regulation, regulate).
nomi(synthesis,	 synthesize).


% Lexicon: adjectives:
lex(anabolic,    adj, inter, predi, const(body_fluid)).
lex(anabolic,    adj, inter, predi, const(body_part)).
lex(circulating, adj, inter, predi, const(_)).
lex(pancreatic,  adj, inter, predi, const(body_part)).
lex(pancreatic,  adj, inter, predi, const(body_fluid)).
lex(skeletal,    adj, inter, predi, const(body_part)).
lex(skeletal,    adj, inter, predi, const(body_fluid)).

lex(high,        adj, subs,  predi, const(_)).
lex(large,       adj, subs,  predi, const(_)).
lex(low,         adj, subs,  predi, const(_)).
lex(small,       adj, subs,  predi, const(_)).
lex(widespread,  adj, subs,  predi, const(_)).


% Lexicon: adverbs
lex(strongly, adv).












