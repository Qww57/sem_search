%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% test_decompose.pl:
%
%		      Unit tests for extended grammar.
%
%
%
% This file aims at testing the basic features that should be handled by
% Natural Logics grammar.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- ensure_loaded(decompose).
:- ensure_loaded(lexicon_insulin).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Helper functions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% have_same_elements(+X,+Y) - returns true if X and Y are composed of
% the same elements.
%
have_same_elements(X,Y) :- !,
	length(X,L), length(Y,L), !,
	intersection(X,Y,Z), length(Z,L).
intersection([X|Tail],Y,[X|Z]) :-
    member(X,Y),
    intersection(Tail,Y,Z).
intersection([X|Tail],Y,Z) :-
    \+ member(X,Y),
    intersection(Tail,Y,Z).
intersection([],_,[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Very simple cases.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(decompose).

test(basic_proposition_rel) :-
	decompose([cell, produce, insulin],R),
	R = [fact(prop,[cell], [produce], [insulin], observation)].

test(basic_proposition_isa) :-
	decompose([insulin,isa,hormone],R),
	R = [isa([insulin], [hormone])].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Respect of NP modifiers leaf decomposition rules.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(basic_pp) :-
	decompose([cell,in,pancreas,produce,insulin],R),
	E = [isa([cell,in,pancreas], [cell]),
	     fact(prep, [cell,in,pancreas], in, [pancreas], _),
	     fact(prop, [cell,in,pancreas], [produce], [insulin], observation)],
	have_same_elements(R,E).

test(basic_rc) :-
	decompose([cell,that,produce,insulin,produce,insulin],R),
	E = [isa([cell,that,produce,insulin], [cell]),
	     fact(rc, [cell,that,produce,insulin], [produce], [insulin], definition),
	     fact(prop, [cell,that,produce,insulin], [produce], [insulin], observation)],
	have_same_elements(R,E).

test(basic_adj_intersective) :-
	decompose([pancreatic,cell,produce,insulin],R),
	E = [isa([pancreatic, cell], [cell]),
	     isa([pancreatic, cell], [pancreatic, entity]),
	     fact(prop, [pancreatic,cell], [produce], [insulin], observation)],
	have_same_elements(R,E).

test(basic_adj_subsective) :-
	decompose([large,cell,produce,insulin],R),
	E = [isa([large, cell], [cell]),
	     fact(prop, [large,cell], [produce], [insulin], observation)],
	have_same_elements(R,E).

test(basic_adj_non_subs) :-
	decompose([former,cell,produce,insulin],R),
	R = [fact(prop, [former,cell], [produce], [insulin], observation)].

test(basic_cn) :-
	decompose([insulin, production, produce, insulin],R),
	E = [isa([insulin, production], [production]),
	     fact(cn, [insulin, production], none, [insulin], _),
	     fact(prop, [insulin, production], [produce], [insulin], observation)],
	have_same_elements(E,R).

test(basic_ger_simple) :-
	decompose([pancreas,s,production, produce, insulin],R),
	E = [isa([pancreas, s, production], [production]),
	     fact(ger, [pancreas, s, production], none, [pancreas], _),
	     fact(prop, [pancreas, s, production], [produce], [insulin], observation)],
	have_same_elements(E,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Respect of NP modifiers general decomposition rules.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test(basic_cn_multiple) :-
	decompose([insulin, pancreas, production, produce, insulin],R),
	E = [isa([insulin, pancreas, production], [insulin, production]),
	     isa([insulin, pancreas, production], [pancreas, production]),
	     isa([pancreas, production], [production]),
	     isa([insulin, production], [production]),
	     fact(cn, [insulin, production], none, [insulin], _),
	     fact(cn, [pancreas, production], none, [pancreas], _),
	     fact(prop, [insulin, pancreas, production],
		  [produce], [insulin], observation)],
	have_same_elements(E,R).

test(basic_modifiers_multiple) :-
	decompose([large,betacell,in,pancreas, produce, insulin],R),
	E = [isa([large, betacell, in, pancreas], [large, betacell]),
	     isa([large, betacell, in, pancreas], [betacell, in, pancreas]),
	     isa([large, betacell], [betacell]),
	     isa([betacell, in, pancreas], [betacell]),
	     fact(prep, [betacell, in, pancreas], in, [pancreas], _),
	     fact(prop, [large, betacell, in, pancreas],
		  [produce], [insulin], observation)],
	have_same_elements(E,R).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Respect of NP extension rules.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% One AC,

% One PC - relation


% One PC - isa.

test(extension_pc_isa_simple) :- % Should remove the extension when treated.
	decompose([pancreas, produce, insulin,',',which,isa,hormone,','],R),
	E = [fact(prop, [pancreas], [produce], [insulin], observation),
	     isa([insulin], [hormone])],
	have_same_elements(E,R).


test(extension_pc_isa_in_rc) :- % Should remove the extension when treated.
	decompose([pancreas,that,produce,fat,',',which,isa,triglyceride,',',
		   regulate,production],R),
	E = [fact(prop, [pancreas, that, produce, fat], [regulate],
		  [production], observation),
	     isa([pancreas, that, produce, fat], [pancreas]),
	     fact(rc, [pancreas, that, produce, fat], [produce], [fat], definition),
	     isa([fat], [triglyceride])],
	have_same_elements(E,R).

% One PC - isa.
test(extension_pc_isa_in_rc_modified) :- % Should remove the extension when treated.
	decompose([pancreas,that,produce,in,liver,fat,',',which,isa,triglyceride,',',
		   regulate,production],R),
	E = [fact(prop, [pancreas, that, produce, in, liver, fat],
		  [regulate], [production], observation),
	     isa([pancreas, that, produce, in, liver, fat], [pancreas]),
	     fact(rc, [pancreas], [produce], [fat], definition),

	     attach(fact(rc, [pancreas], [produce], [fat], definition),
		    [production, of, fat, by, pancreas, in, liver]),
	     isa([fat], [triglyceride]),
	     fact(prep, [production, of, fat], of, [fat], patient),
	     isa([production, of, fat], [production]),
	     fact(prep, [production, by, pancreas], by, [pancreas], agent),
	     isa([production, by, pancreas], [production]),
	     fact(prep, [production, in, liver], in, [liver], location),
	     isa([production, in, liver], [production]),
	     isa([production, of, fat, by, pancreas, in, liver],
		 [production, by, pancreas, in, liver]),
	     isa([production, of, fat, by, pancreas, in, liver],
		 [production, of, fat, by, pancreas]),
	     isa([production, of, fat, by, pancreas, in, liver],
		 [production, of, fat, in, liver]),
	     isa([production, by, pancreas, in, liver],
		 [production, by, pancreas]),
	     isa([production, by, pancreas, in, liver],
		 [production, in, liver]),
	     isa([production, of, fat, by, pancreas],
		 [production, by, pancreas]),
	     isa([production, of, fat, by, pancreas],
		 [production, of, fat]),
	     isa([production, of, fat, in, liver],
		 [production, in, liver]),
	     isa([production, of, fat, in, liver],
		 [production, of, fat])],

	M = [fact(prop, [pancreas, that, produce, in, liver, fat],
		  [regulate], [production], observation),
	     isa([pancreas, that, produce, in, liver, fat], [pancreas]),
	     fact(rc, [pancreas], [produce], [fat], definition),
	     attach(fact(rc, [pancreas], [produce], [fat], definition),
		    [production, of, fat, and, by, pancreas, and, in, liver]),
	     isa([fat], [triglyceride]),
	     fact(prep, [production, of, fat], of, [fat], patient),
	     isa([production, of, fat], [production]),
	     fact(prep, [production, by, pancreas], by, [pancreas], agent),
	     isa([production, by, pancreas], [production]),
	     fact(prep, [production, in, liver], in, [liver], location),
	     isa([production, in, liver], [production]),
	     isa([production, of, fat, and, by, pancreas, and, in, liver],
		 [production, by, pancreas, and, in, liver]),
	     isa([production, of, fat, and, by, pancreas, and, in, liver],
		 [production, of, fat, and, by, pancreas]),
	     isa([production, of, fat, and, by, pancreas, and, in, liver],
		 [production, of, fat, and, in, liver]),
	     isa([production, by, pancreas, and, in, liver],
		 [production, by, pancreas]),
	     isa([production, by, pancreas, and, in, liver],
		 [production, in, liver]),
	     isa([production, of, fat, and, by, pancreas],
		 [production, by, pancreas]),
	     isa([production, of, fat, and, by, pancreas],
		 [production, of, fat]),
	     isa([production, of, fat, and, in, liver],
		 [production, in, liver]),
	     isa([production, of, fat, and, in, liver],
		 [production, of, fat])],
	have_same_elements(E,R).


% Do also failing cases because not implemented.

% Add case where it is nested far.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Respect of adverbials PPs.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



:- end_tests(decompose).











