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
lex(absorption,    noun, creation).
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
lex(isa,        trans, copular, is_some,     const(X,X)).

lex(affect,     trans, event,   affected,    const([_],[_])).
lex(absorb,     trans, event,   absorbed,    const([body_part, process],
						   [body_fluid])).
lex(convert,    trans, event,   converted,   const([body_part, transformation],
						   [body_fluid])).
lex(excrete,    trans, event,   excreted,    const([creative, body_part],
						   [body_fluid])).
lex(inhibit,    trans, event,   inhibited,   const([process, quality,
						    body_part, body_fluid],
						   [process])).
lex(produce,    trans, event,   produced,    const([body_part, creation],
						   [body_fluid])).
lex(promote,    trans, event,   promoted,    const([process, quality,
						    body_part, body_fluid],
						   [process])).
lex(regulate,   trans, event,   regulated,   const([process, quality,
						    body_part, body_fluid],
						   [process, state])).
lex(synthesize, trans, event,   synthesized, const([process],
						   [body_fluid])).

nomi(absorption, absorb).
nomi(excretion,  excrete).
nomi(conversion, convert).
nomi(impact,     affect).
nomi(production, produce).
nomi(promotion,  promote).
nomi(regulation, regulate).
nomi(synthesis,	 synthesize).


% Lexicon: adjectives:
lex(anabolic,    adj, inter, predi, const([body_fluid, body_part])).
lex(circulating, adj, inter, predi, const([_])).
lex(pancreatic,  adj, inter, predi, const([body_fluid, body_part])).
lex(skeletal,    adj, inter, predi, const([body_fluid, body_part])).

lex(high,        adj, subs,  predi, const([_])).
lex(large,       adj, subs,  predi, const([_])).
lex(low,         adj, subs,  predi, const([_])).
lex(small,       adj, subs,  predi, const([_])).
lex(widespread,  adj, subs,  predi, const([_])).


% Lexicon: adverbs
lex(strongly, adv).












