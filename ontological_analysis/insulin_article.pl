
:- [extended_grammar].
:- [insulin_lexicon].

% First paragraph on wikipedia article on insulin.

sentence(10, [insulin, isa, peptide, hormone, that, is, produced, by, betacell,
	     of, pancreatic, islet]).

sentence(20, [insulin,
	     regulate, by, promotion, of, absorption, of, glucose, from, blood,
	     into, fat, into, liver, and, into, skeletal, muscle,
	     metabolism, of, carbohydrate]).

sentence(21, [insulin,
	     regulate, by, promotion, of, absorption, of, glucose, from, blood,
	     into, fat, into, liver, and, into, skeletal, muscle,
	     metabolism, of, fat]).

sentence(22, [insulin,
	     regulate, by, promotion, of, absorption, of, glucose, from, blood,
	     into, fat, into, liver, and, into, skeletal, muscle,
	     metabolism, of, protein]).

sentence(30, [glucose,
	      is, converted, in, fat, ',', in, liver, and, in, skeletal, muscle,
	      cell, into,
	      glycogen, and, into, fat, ',', which, isa, triglyceride, ',']).

sentence(40, [glucose, production, by, liver,
	      is, inhibited, by,
	      high, insulin, concentration, in, blood]).

sentence(41, [glucose, excretion, into, blood, by, liver,
	      is, inhibited, by,
	      high, insulin, concentration, in, blood]).

sentence(50, [circulating, insulin,
	      affect,
	      synthesis, in, tissue, of, protein]).

sentence(60, [insulin,
	      isa, at, high, insulin, concentration, in, blood,
	      anabolic, hormone, that, promote, conversion, of, small, molecule,
	      in, blood, into, large, molecule, in, cell]).

sentence(70, [insulin,
	      promote, at, low, insulin, concentration,
	      widespread, catabolism]).

:- begin_tests(insulin).

test(sentence_1_0) :- sentence(10,X), p(_,X,[]).
test(sentence_2_0) :- sentence(20,X), p(_,X,[]).
test(sentence_2_1) :- sentence(21,X), p(_,X,[]).
test(sentence_2_2) :- sentence(22,X), p(_,X,[]).
test(sentence_3_0) :- sentence(30,X), p(_,X,[]).
test(sentence_4_0) :- sentence(40,X), p(_,X,[]).
test(sentence_4_1) :- sentence(41,X), p(_,X,[]).
test(sentence_5_0) :- sentence(50,X), p(_,X,[]).
test(sentence_6_0) :- sentence(60,X), p(_,X,[]).
test(sentence_7_0) :- sentence(70,X), p(_,X,[]).

:- end_tests(insulin).
