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
lex(as,             preposition).
lex(at,             preposition).
lex(by,		    preposition).
lex(in,		    preposition).
lex(in_response_to, preposition).
lex(into,           preposition).
lex(from,           preposition).
lex(of,	            preposition).
lex(within,	    preposition).


% Lexicon: nouns
lex(absorption,      noun, process).
lex(alphacell,       noun, cell).
lex(betacell,        noun, cell).
lex(blood,           noun, body_fluid).
lex(carbohydrate,    noun, body_fluid).
lex(catabolism,      noun, process).
lex(cell,            noun, cell).
lex(circulation,     noun, material).
lex(concentration,   noun, quantity).
lex(conversion,	     noun, transformation).
lex(cue,             noun, information).
lex(effect,          noun, process).
lex(excretion,       noun, creation).
lex(fat,             noun, body_fluid).
lex(fluid,           noun, body_fluid).
lex(glucagon,        noun, body_fluid).
lex(gluconeogenesis, noun, creation).
lex(glucose,	     noun, body_fluid).
lex(glycogen,	     noun, body_fluid).
lex(glycogenolysis,  noun, creation).
lex(hormone,         noun, protein).
lex(impact,          noun, enhancement).
lex(insulin,         noun, protein).
lex(islet,           noun, body_part).
lex(keeping,         noun, process).
lex(limit,	     noun, quantity).
lex(liver,           noun, organ).
lex(manner,	     noun, quality).
lex(mechanism,       noun, process).
lex(metabolism,      noun, process).
lex(molecule,        noun, mass).
lex(muscle,          noun, body_part).
lex(pancreas,        noun, organ).
lex(peptide,         noun, protein).
lex(plasma,	     noun, material).
lex(promotion,       noun, enhancement).
lex(production,      noun, creation).
lex(protein,         noun, protein).
lex(regulation,      noun, enhancement).
lex(release,         noun, process).
lex(secretion,	     noun, creation).
lex(stimulation,     noun, enhancement).
lex(stop,            noun, enhancement).
lex(synthesis,       noun, creation).
lex(taking,          noun, process).
lex(tissue,          noun, body_part).
lex(triglyceride,    noun, body_fluid).


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
lex(keep,       trans, event,	kept,        const([process, quality,
						    body_part, body_fluid],
						   [process, quality,
						    body_part, body_fluid])).
lex(produce,    trans, event,   produced,    const([body_part, creation],
						   [body_fluid])).
lex(produce,    trans, event,   produced,    const([quality], [entity])). % Abstract sense
lex(promote,    trans, event,   promoted,    const([process, quality,
						    body_part, body_fluid],
						   [process])).
lex(stop,	trans, event,   stopped,     const([process, body_part],
						   [process, state])).
lex(release,	trans, event,   released,    const([body_part, process],
						   [body_fluid])).
lex(regulate,   trans, event,   regulated,   const([process, quality,
						    body_part, body_fluid],
						   [process, state])).
lex(secrete,    trans, event,   secreted,    const([body_part, creation],
						   [body_fluid])).
lex(stimulate,  trans, event,   stimulated,  const([process, quality,
						    body_part, body_fluid],
						   [process, state])).
lex(synthesize, trans, event,   synthesized, const([process],
						   [body_fluid])).
lex(take,       trans, event,   taken,       const(_,_)).

% Equivalence verbs - nouns.
nomi(_,           isa).
nomi(absorption,  absorb).
nomi(excretion,   excrete).
nomi(conversion,  convert).
nomi(impact,      affect).
nomi(keeping,     keep).
nomi(production,  produce).
nomi(promotion,   promote).
nomi(regulation,  regulate).
nomi(release,	  release).
nomi(secretion,   secrete).
nomi(stimulation, stimulate).
nomi(stop,        stop).
nomi(synthesis,	  synthesize).
nomi(taking,	  take).


% Lexicon: adjectives:
lex(anabolic,      adj, inter, predi, const([body_fluid, body_part])).
lex(extracellular, adj, inter, predi, const([body_fluid, body_part])).
lex(circulating,   adj, inter, predi, const([_])).
lex(pancreatic,    adj, inter, predi, const([body_fluid, body_part])).
lex(skeletal,      adj, inter, predi, const([body_fluid, body_part])).

lex(general,       adj, subs,  predi, const([_])).
lex(high,          adj, subs,  predi, const([_])).
lex(large,         adj, subs,  predi, const([_])).
lex(low,           adj, subs,  predi, const([_])).
lex(narrow,	   adj, subs,  predi, const([_])).
lex(powerful,      adj, subs,  predi, const([_])).
lex(small,         adj, subs,  predi, const([_])).
lex(strong,        adj, subs,  predi, const([_])).
lex(widespread,    adj, subs,  predi, const([_])).

lex(opposite,      adj, non_subs, predi,     const([process, quality])).
lex(primary,	   adj, non_subs, non_predi, const([_])).


% Lexicon: adverbs
lex(highly,      adv).
lex(lowly,       adv).
lex(powerfully,  adv).
lex(strongly,	 adv).

% Equivalence adjectives - adverbs.
adv_adj(highly,     high).
adv_adj(lowly,      low).
adv_adj(powerfully, powerful).
adv_adj(strongly,   strong).













