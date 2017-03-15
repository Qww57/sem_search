%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% GRAMMAR.pl:
%	      Syntactic grammar for natural logics propositions.
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [lexicon].

proposition(s(NP,VP)) --> np(NP), vp(VP).

vp(vp(V,C))--> rterm(V), np(C).

np(np(N,comp(C))) --> ['{'], noun(N), comp(C), ['}'].
np(np(N,comp([]))) --> noun(N).

comp([rc(VP)|T]) --> [that], vp(VP), [and], comp(T).
comp([pp(H1,H2)|T]) --> prep(H1), np(H2), [and], comp(T).
comp([rc(VP)]) --> [that], vp(VP).
comp([pp(H1,H2)]) --> prep(H1), np(H2).

pps([pp(H1,H2)|T]) --> prep(H1), np(H2), [and], pps(T).
pps([pp(H1,H2)]) --> prep(H1), np(H2).

rterm(isa) --> [isa].
rterm(verb(V)) --> verb(V).
rterm(verb(V,pps(P))) --> verb(V), pps(P).
rterm(rpas(V)) --> [is], verbpp(V),[by].
rterm(rpas(V,pps(P))) --> [is], verbpp(V), pps(P),[by].
rterm(radv(V,P)) --> [is], verbpp(V), prep(P).

noun(N) --> [N], {lexicon(N, noun)}.
prep(P) --> [P], {lexicon(P, prep)}.
verb(V) --> [V], {lexicon(V, verb, _)}.
verbpp(V) --> [V], {lexicon(_, verb, V)}.


inv_verb(X,Y) :- lexicon(X,verb,Y).







