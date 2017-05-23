:- ensure_loaded(extended_grammar).

:- begin_tests(grammar).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Respect of construction rules for pre and post-modifiers.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
test(pre_rc_3) :- np(_,[pancreas,that,produce,glucose,and,that,produce,insulin],[]).

% Use of post-modifiers: Appositions.


% Syntax order of modifiers:
test(mod1) :- np(_,[pancreas,s,synchronous,glucose,production,of,glucose,and,
		  that,produce,glucose],[]).
test(mod2, fail) :- np(_,[pancreas,s,glucose,synchronous,production,of,glucose],[]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     Respect of syntactic rules related to verb usage.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Respect of transitivity in the main proposition.
test(trans_1) :- p([_],[doctor,smile],[]). % Normal use of intransitive verb.
test(trans_2) :- p([_],[doctor,observe],[]). % Use of transitive verb as intrans verb.
test(trans_3) :- p([_],[doctor,observe,pancreas],[]). % Normal use of transitive verb.
test(trans_4, fail) :- p([_], [doctor,smile,pancreas],[]). % Use of intrans as transitive.

% Respect of transitivity in RC.
test(trans_rc_1) :- np(_,[pancreas,that,smile],[]).
test(trans_rc_2) :- np(_,[pancreas,that,produce],[]).
test(trans_rc_3) :- np(_,[pancreas,that,produce,insulin],[]).
test(trans_rc_4, fail):- np(_,[pancreas,that,smile,insulin],[]).

% Respect of predicativity for adjectives.
test(adj_pred_1) :- p([_],[doctor,is,young],[]).
test(adj_pred_1) :- p([_],[doctor,is,young,and,red],[]).
test(adj_pred_2, fail) :- p([_],[doctor,is,former],[]). % Non predicate adjective.
test(adj_pred_3, fail) :- p([_],[doctor,produce,former],[]). % Non copular verb.
test(adj_pred_4, fail) :- p([_],[doctor,produce,young],[]). % Non copular verb.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%     Respect of syntactic rules related to proposition extensions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Respect of plural readings.
test(plural_1, Z==2) :- p(X,[alphacell,and,betacell,produce,insulin],[]),
	          length(X,Z). % Distributive reading on subject.
test(plural_2, Z==2) :- p(X,[alphacell,produce,insulin,and,glucose],[]),
	          length(X,Z). % Distributive reading on object
test(plural_3, Z==2) :- p(X,[alphacell,and,betacell,produce,insulin,and,glucose],[]),
	          length(X,Z). % Respective reading on subject and object.
test(plural_4, fail) :- p(_,[alphacell,or,betacell,produce,insulin],[]). % Not handled.
test(plural_5, fail) :- p(_,[alphacell,produce,insulin,or,glucose],[]). % No supremum.
test(plural_6) :- p([_],[insulin,is,produced,by,alphacell,or,betacell],[]). % Supremum.

:- end_tests(grammar).












