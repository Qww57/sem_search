%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% test_reverse_grammar.pl:
%
%	       Helper functions in order to convert parse trees
%                      to strings (list of words).
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- ensure_loaded([grammar]).
:- ensure_loaded([reverse_grammar]).


% Concerning the unparsing, it is important to check that the successive
% parsings and unparsings are not affecting the tree structure.
% Explicit alignment should then be used when needed.
%
% For instance, the following scenario is to avoid:
%
% ?- np(X,_,[liver,release,of,glucagon,in,blood,by,glycogenolysis,and,by,
% gluconeogenesis],[]), reverse_np(X,Y), np(M,_,Y,[]).
%
% X = np(n(release),
%	 mod([cn([agent], n(liver)),
%	      pp(of,[patient], np(n(glucagon), mod([]), ext([]))),
%             pp(in,[location], np(n(blood), mod([]), ext([]))),
%             pp(by,[manner],np(n(glycogenolysis), mod([]), ext([]))),
%	      pp(by,[manner],np(n(gluconeogenesis),mod([]),ext([])))]),
%        ext([])),
%
% Y = [liver, release, of, glucagon, in, blood, by, glycogenolysis, by,
%      gluconeogenesis],
%
% M = np(n(release),
%        mod([cn([agent], n(liver)),
%	      pp(of,[patient], np(n(glucagon), mod([]), ext([]))),
%	      pp(in,[location], np(n(blood), mod([]), ext([]))),
%	      pp(by,[manner], np(n(glycogenolysis),
%				 mod([pp(by,[manner],np(n(gluconeogenesis),
%							mod([]),
%						        ext([])))]),
%	                         ext([])))]),
%	 ext([])) .
%
% As a consequence, the predicate reverse_np has then been implemented
% in a way that is does not only unparse, but also checks that the
% parsing preserves the meaning.
%
%
test_np(X) :-
	np(T1,Z,X,[]),      % Parse noun phrase.
	reverse_np(T1,X2),  % Convert it back to string.
	np(T2,Z,X2,[]), !,  % Parse new noun phrase.
	T2 = T1,	    % Verify that is unchanged.
	reverse_np(T2,X3),  % Convert it back to string.
	np(T3,Z,X3,[]), !,  % Parse new noun phrase.
	T3 = T1.            % Verify that is unchanged.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests(reverse_grammar).

% Testing simple case.
test(simple) :- test_np([cell]).

% Testing leaf modifiers.
test(pp) :- test_np([cell,in,pancreas]).
test(rc_active) :- test_np([cell,that,produce,insulin]).
test(rc_passive) :- test_np([insulin,that,is,produced,by,cell]).
test(rc_active_mod) :- test_np([cell,that,produce,in,pancreas,insulin]).
test(rc_passive_mod) :- test_np([insulin,that,is,produced,in,
				 pancreas,by,cell]).
test(adj) :- test_np([pancreatic,cell]).
test(cn) :- test_np([glucose, production]).
test(ger_1) :- test_np([pancreas,s,production]).
test(ger_2) :- test_np([blood,plasma,s,production]).

% Testing multiple PPs.
test(multiple_1) :- % Explicit alignment.
	test_np([glucose,production,in,pancreas]).
test(multiple_2) :- % Implicit alignment.
	test_np([production,by,liver,of,glucose]).
test(multiple_3) :- % Explicit alignment.
	test_np([production,by,liver,and,of,glucose]).
test(multiple_4) :- % Example of nested structure.
	test_np([production,of,glucose,in,blood]).
test(multiple_5) :- % Example from the comments.
	test_np([liver,release,of,glucagon,in,blood,
		 by,glycogenolysis,and,by,gluconeogenesis]).

% Testing extensions.
test(apposition) :- test_np([insulin,',',a,hormone,',']).
test(apposition) :- test_np([insulin,',',which,isa,hormone,',']).
test(apposition) :- test_np([betacell,',',which,produce,hormone,',']).

:- end_tests(reverse_grammar).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%












