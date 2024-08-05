/*
A three-sided die is repeatedly thrown until the outcome is three.
on(T,F) means that on the Tth throw the face F came out.
From
J. Vennekens, S. Verbaeten, and M. Bruynooghe. Logic programs with annotated 
disjunctions. In International Conference on Logic Programming, 
volume 3131 of LNCS, pages 195-209. Springer, 2004.
*/
:- use_module(library(mcintyre)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- endif.
:-dynamic on/2.
:- mc.

:- begin_lpad.

% on(T,F) means that the die landed on face F at time T

on(0,1):1/3;on(0,2):1/3;on(0,3):1/3.
% at time 0 the dice lands on one of its faces with equal probability

on(X,1):1/3;on(X,2):1/3;on(X,3):1/3:-
  X1 is X-1,X1>=0,
  on(X1,_),
  \+ on(X1,3).
% at time T the die lands on one of its faces with equal probability if
% at the previous time point it was thrown and it did not land on face 6

evidence:-
  on(0,1),
  on(1,1).

int(0).

int(X1):-int(X),X1 is X+1.

at_least_once_1:- int(X),((on(X,3),!,fail);on(X,1)).

never_1:- \+ at_least_once_1.

:- end_lpad.

/** <examples>

?- mc_sample(at_least_once_1,1000,Prob,[successes(S),failures(F)]). % what is the probability that the die lands on face 1 at least once?
% expected result 0.5
?- mc_sample(never_1,1000,Prob,[successes(S),failures(F)]). % what is the probability that the die never lands on face 1?
% expected result 0.5
?- mc_prob(on(0,1),Prob),bar(Prob,C). % what is the probability that the die lands on face 1 at time 0?
% expected result 0.333333333333333
?- mc_prob(on(1,1),Prob),bar(Prob,C). % what is the probability that the die lands on face 1 at time 1?
% expected result 0.222222222222222
?- mc_prob(on(2,1),Prob),bar(Prob,C). % what is the probability that the die lands on face 1 at time 2?
% expected result 0.148148147703704
?- mc_mh_sample(on(2,1),on(1,1),1000,P,[mix(1000),successes(T),failures(F)]).
% expected result 0.333333333333333
?- mc_mh_sample(on(2,1),on(0,1),1000,P,[mix(1000),successes(T),failures(F)]).
% expected result 0.222222222222222
?- mc_gibbs_sample(on(0,1),1000,P).
% expected result 0.333333333333333
?- mc_gibbs_sample(on(1,1),1000,P).
% expected result 0.222222222222222
?- mc_gibbs_sample(on(2,1),1000,P).
% expected result 0.148148147703704
?- mc_gibbs_sample(on(2,1),on(1,1),1000,P,[mix(1000),successes(T),failures(F)]).
% expected result 0.333333333333333
?- mc_gibbs_sample(on(2,1),on(0,1),1000,P,[mix(1000),successes(T),failures(F)]).
% expected result 0.222222222222222

*/
 
