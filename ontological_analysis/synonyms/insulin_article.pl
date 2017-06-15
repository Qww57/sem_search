%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Insulin_article.pl:
%
%	    Examples from the two first paragraphs of wikipedia's
%                          article on insulin.
%
%
%
% This file contains Natural Logics propositions taken from the two
% first paragraphs of the wikipedia article on insulin (version of
% the 16/11/2016). Each proposition example has an ID composed of
% three digits. The first one refers to the paragraph (1 or 2). The
% second refers to the index of the sentence in the original paragraph.
% The third one is used to distinguished sub-propositions when the
% original sentence has been split in simpler propositions.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% First paragraph.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sentence(110, [insulin, isa, peptide, hormone, that, is, produced, by, betacell,
	      of, pancreatic, islet]).

sentence(120, [insulin,
	      regulate, by, promotion, of, absorption, of, glucose, from, blood,
	      into, fat, into, liver, and, into, skeletal, muscle,
	      metabolism, of, carbohydrate]).

sentence(121, [insulin,
	      regulate, by, promotion, of, absorption, of, glucose, from, blood,
	      into, fat, into, liver, and, into, skeletal, muscle,
	      metabolism, of, fat]).

sentence(122, [insulin,
	      regulate, by, promotion, of, absorption, of, glucose, from, blood,
	      into, fat, into, liver, and, into, skeletal, muscle,
	      metabolism, of, protein]).

sentence(130, [absorbed, glucose,
	      is, converted, in, fat, ',', in, liver, and, in, skeletal, muscle,
	      cell, into, glycogen, by, glycogenesis]).

sentence(131, [absorbed, glucose,
	      is, converted, in, fat, ',', in, liver, and, in, skeletal, muscle,
	      cell, into, fat, ',', which, isa, triglyceride, ',', by, lipogenesis]).

sentence(132, [absorbed, glucose,
	      is, converted, in, liver, into, glycogen, and, into, fat]).

sentence(140, [glucose, production, by, liver,
	      is, inhibited, by,
	      high, insulin, concentration, in, blood]).

sentence(141, [glucose, excretion, into, blood, by, liver,
	      is, inhibited, by,
	      high, insulin, concentration, in, blood]).

sentence(150, [circulating, insulin,
	      affect,
	      synthesis, in, tissue, of, protein]).

sentence(160, [insulin,
	      isa, at, high, insulin, concentration, in, blood,
	      anabolic, hormone, that, promote, conversion, of, small, molecule,
	      in, blood, into, large, molecule, in, cell]).

sentence(170, [insulin,
	      promote, at, low, insulin, concentration,
	      widespread, catabolism]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Second paragraph.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sentence(210, [pancreatic, betacell, is, affected, by, glucose, concentration, in,
	       blood]).

sentence(220, [betacell, secrete, at, high, glucose, concentration, into, blood,
	       insulin]).

sentence(221, [betacell, stop, at, low, glucose, concentration, insulin, secretion,
	       into, general, circulation]).

sentence(230, [alphacell, take,	from, betacell, cue]).

sentence(231, [alphacell, secrete, into, blood, and, in, opposite, manner, as, betacell,
	       glucagon]).

sentence(232, [alphacell, secrete, highly, at, high, glucose, concentration, glucagon]).

sentence(233, [alphacell, secrete, lowly, at, low, glucose, concentration, glucagon]).

sentence(240, [high, glucagon, concentration, in, blood, plasma, stimulate, powerfully,
	       liver, release, of, glucagon, in, blood, by, glycogenolysis, and, by,
	       gluconeogenesis]).

sentence(241, [stimulation, of, liver, glucose, release, in, blood, by,
	       glycogenolysis, and, by, gluconeogenesis, affect, in, opposite, manner,
	       as, effect, that, is, produced, by, high, insulin, concentration,
	       blood, glucose, concentration]).

sentence(250, [secretion, of, insulin, into, blood, in_response_to, blood, glucose,
	       concentration, isa, primary, mechanism, that, keep, within,
	       narrow, limit, glucose, concentration, in, extracellular, fluid]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%











