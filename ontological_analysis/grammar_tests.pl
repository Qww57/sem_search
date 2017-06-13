%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Grammar_tests.pl:
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

:- ensure_loaded(extended_grammar). % Loading the extended grammar.
:- ensure_loaded(basic_lexicon).    % Loading the example lexicon.


:- begin_tests(grammar).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Respect of construction rules for pre and post-modifiers.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Use of pre-modifiers: adjectives.
test(pre_adj_1) :- np(_,[synchronous,production],[]). % Use of an adjectives.
test(pre_adj_2) :- np(_,[synchronous,former,production],[]). % Use of multiple adj.
test(pre_adj_3, fail) :- np(_,[synchronous,and,former,production],[]). % No keyword and.

% Use of pre-modifiers: compound nouns.
test(pre_cn_1) :- np(_,[glucose,production],[]). % Normal compound noun with 2 words.
test(pre_cn_2) :- np(_,[pancreas,glucose,production],[]). % Compound noun with 3 words.
test(pre_cn_3, fail) :- np(_,[produce,production],[]). % Compound noun with verb.

% Use of pre-modifiers: possessives
test(pre_cn_1) :- np(_,[pancreas,s,production],[]). % Normal possessive with noun.
test(pre_cn_2, fail) :- np(_,[produce,s,production],[]). % Possessive with verb.
test(pre_cn_3) :- np(_,[glucose,production,s,behaviour],[]). % Possessive with comp noun.

% Use of post-modifiers: PP.
test(post_pp_1) :- np(_,[production,of,glucose],[]). % Normal use of PP.
test(post_pp_2, fail) :- np(_,[production,af,glucose],[]). % PP with wrong preposition.
test(post_pp_3, fail) :- np(_,[production,of,produce],[]). % PP with verb after prep.
test(post_pp_4) :- np(_,[production,of,glucose,in,pancreas],[]).

% Use of post-modifiers: RC.
test(post_rc_1) :- np(_,[pancreas,that,produce,glucose],[]). % Normal use of RC.
test(post_rc_2, fail) :- np(_,[pancreas,that,glucose],[]). % Wrong use of RC.
test(post_rc_3, fail) :- np(_,[cell,that,isa,betacell],[]).
test(post_rc_4, fail) :- np(_,[cell,that,isa,in,pancreas,betacell],[]).
test(post_rc_5) :- np(_,[cell,that,produce,in,pancreas,glucose],[]).
test(post_rc_6) :- np(_,[insulin,that,is,produced,by,betacell],[]).
test(post_rc_7) :- np(_,[insulin,that,is,produced,in,pancreas,by,betacell],[]).
test(post_rc_8) :- np(_,[insulin,that,is,produced],[]).
test(post_rc_9) :- np(_,[insulin,that,is,produced,in,pancreas],[]).
test(post_rc_10) :- np(_,[pancreas,that,produce,glucose,and,that,produce,insulin],[]).

% Use of post-modifiers: PC.
test(post_pc_1) :- np(_,[pancreas,',',which,produce,glucose,','],[]).
test(post_pc_2, fail) :- np(_,[pancreas,',',which,produce,','],[]).

% Use of post-modifiers: Appositions.
test(post_ap_1) :- np(_,[pancreas,',',an,insulin,','],[]).
test(post_ap_2) :- np(_,[pancreas,',',a,insulin,','],[]).
test(post_ap_3, fail) :- np(_,[pancreas,',',a,insulin],[]).

% Syntax order of modifiers:
test(mod1) :- np(_,[pancreas,s,synchronous,glucose,production,of,glucose,and,
		  that,produce,glucose],[]).
test(mod2, fail) :- np(_,[pancreas,s,glucose,synchronous,production,of,glucose],[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Respect of syntactic rules related to verb usage.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Respect of transitivity in the main proposition.
test(trans_1, fail) :- p([_],[doctor,smile],[]). % No use of intransitive verb.
test(trans_2, fail) :- p([_],[doctor,observe],[]). % No use of trans verb without object.
test(trans_3) :- p([_],[doctor,observe,pancreas],[]). % Normal use of transitive verb.
test(trans_4, fail) :- p([_], [doctor,smile,pancreas],[]). % No use of intrans as trans.

% Respect of transitivity in RC.
test(trans_rc_1, fail) :- np(_,[pancreas,that,smile],[]).
test(trans_rc_2, fail) :- np(_,[pancreas,that,produce],[]).
test(trans_rc_3) :- np(_,[pancreas,that,produce,insulin],[]).
test(trans_rc_4, fail):- np(_,[pancreas,that,smile,insulin],[]).

% Ability to use passive forms, even without object (agent).
test(pass_1) :- p([_],[insulin,is,produced,by,pancreas],[]).
test(pass_2) :- p([_],[insulin,is,produced,in,pancreas,by,pancreas],[]).
test(pass_3) :- p([_],[insulin,is,produced],[]).
test(pass_4) :- p([_],[insulin,is,produced,in,pancreas],[]).

% Respect of predicativity for adjectives.
test(adj_pred_1) :- p([_],[doctor,isa,young],[]).
test(adj_pred_2) :- p([_],[doctor,isa,young,red],[]).
test(adj_pred_3, fail) :- p([_],[doctor,isa,former],[]). % Non predicate adjective.
test(adj_pred_4, fail) :- p([_],[doctor,produce,former],[]). % Non copular verb.
test(adj_pred_5, fail) :- p([_],[doctor,produce,young],[]). % Non copular verb.

% Respect of rules regarding adverbs.
test(adv_1) :- p([_],[betacell,produce,in,pancreas,insulin],[]).
test(adv_2) :- p([_],[betacell,produce,synchronously,insulin],[]).
test(adv_3) :- p([_],[betacell,produce,synchronously,in,pancreas,insulin],[]).
test(adv_4, fail) :- % Wrong order of adverbs and adverbial PPs
	p([_],[betacell,produce,in,pancreas,synchronously,insulin],[]).
test(adv_5, fail) :- % Wrong position of the adverb.
	p([_],[betacell,synchronously,produce,insulin],[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Respect of syntactic rules related to proposition extensions.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Respect of plural readings.
test(plural_1, Z==2) :- p(X,[alphacell,and,betacell,produce,insulin],[]),
	          length(X,Z). % Distributive reading on subject.
test(plural_2, Z==2) :- p(X,[alphacell,produce,insulin,and,glucose],[]),
	          length(X,Z). % Distributive reading on object
test(plural_3, Z==4) :- p(X,[alphacell,and,betacell,produce,insulin,and,glucose],[]),
	          length(X,Z). % Double distributive reading on subject and object.
test(plural_4, fail) :- p(_,[alphacell,or,betacell,produce,insulin],[]). % Not handled.
test(plural_5, fail) :- p(_,[alphacell,produce,insulin,or,glucose],[]). % No supremum.
test(plural_6) :- p([_],[insulin,is,produced,by,alphacell,or,betacell],[]). % Supremum.


:- end_tests(grammar).













