%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Insulin_lexicon.pl:
%
%	       Dedicated lexicon for the two first paragraphs of
%		   the wikipedia article on insulin.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- discontiguous lex/2.
:- discontiguous lex/4.
:- discontiguous nomi/2.


% Lexicon: prepositions
lex(as,		    preposition).
lex(at,             preposition).
lex(by,             preposition).
lex(from,	    preposition).
lex(in,		    preposition).
lex(in_response_to, preposition).
lex(into,	    preposition).
lex(of,             preposition).
lex(within,	    preposition).


% Lexicon: nouns
lex(absorption,      noun).
lex(alphacell,       noun).
lex(betacell,        noun).
lex(blood,           noun).
lex(carbohydrate,    noun).
lex(catabolism,      noun).
lex(cell,            noun).
lex(circulation,     noun).
lex(concentration,   noun).
lex(conversion,	     noun).
lex(cue,             noun).
lex(effect,          noun).
lex(excretion,       noun).
lex(fat,             noun).
lex(fluid,           noun).
lex(glucagon,        noun).
lex(gluconeogenesis, noun).
lex(glucose,	     noun).
lex(glycogen,	     noun).
lex(glycogenesis,    noun).
lex(glycogenolysis,  noun).
lex(hormone,         noun).
lex(impact,          noun).
lex(insulin,         noun).
lex(islet,           noun).
lex(keeping,         noun).
lex(limit,           noun).
lex(lipogenesis,     noun).
lex(liver,           noun).
lex(manner,          noun).
lex(mechanism,       noun).
lex(metabolism,      noun).
lex(molecule,        noun).
lex(muscle,          noun).
lex(pancreas,	     noun).
lex(peptide,         noun).
lex(plasma,          noun).
lex(promotion,       noun).
lex(production,      noun).
lex(protein,         noun).
lex(regulation,      noun).
lex(release,         noun).
lex(secretion,	     noun).
lex(stimulation,     noun).
lex(stop,            noun).
lex(synthesis,       noun).
lex(taking,	     noun).
lex(tissue,          noun).
lex(triglyceride,    noun).


% Lexicon: verbs
lex(isa,        trans, copular, is_some).

lex(affect,     trans, event,   affected).
lex(absorb,     trans, event,   absorbed).
lex(convert,    trans, event,   converted).
lex(excrete,    trans, event,   excreted).
lex(inhibit,    trans, event,   inhibited).
lex(keep,	trans, event,   kept).
lex(produce,    trans, event,   produced).
lex(promote,    trans, event,	promoted).
lex(regulate,   trans, event,   regulated).
lex(secrete,	trans, event,   secreted).
lex(stimulate,  trans, event,   stimulated).
lex(stop,	trans, event,   stopped).
lex(synthesize, trans, event,   synthesized).
lex(take,	trans, event,   taken).

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
lex(absorbed,      adj, inter, predi).
lex(anabolic,      adj, inter, predi).
lex(extracellular, adj, inter, predi).
lex(circulating,   adj, inter, predi).
lex(pancreatic,    adj, inter, predi).
lex(skeletal,      adj, inter, predi).

lex(general,       adj, subs,  predi).
lex(high,          adj, subs,  predi).
lex(large,         adj, subs,  predi).
lex(low,           adj, subs,  predi).
lex(narrow,	   adj, subs,  predi).
lex(powerful,      adj, subs,  predi).
lex(small,         adj, subs,  predi).
lex(strong,        adj, subs,  predi).
lex(widespread,    adj, subs,  predi).

lex(opposite,      adj, non_subs, predi).
lex(primary,	   adj, non_subs, non_predi).


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

