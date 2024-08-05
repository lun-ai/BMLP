/* Shop dataset from
Fabrizio Riguzzi and Nicola Di Mauro. Applying the information bottleneck
to statistical relational learning. Machine Learning, 86(1):89-114, 2012.
Meert, W., Struyf, J., and Blockeel, H. 2008.
Learning ground CP-Logic theories by leveraging Bayesian network learning
techniques. Fundamenta Informaticae 89, 131-160

The training examples are all possible worlds of the target programi (shop4).
The prob fact in each model/interpretation/world indicates its probability
(it can be interpreted as frequency in a sampled dataset).
The task is to recover
the values of the parameters of the input program. When learning, the initial
parameters are randomly set. A test set is also provided generated randomly from
the target program.

*/
/** <examples>
?- induce_par([train],P),test(P,[test],LL,AUCROC,ROC,AUCPR,PR).  % learn the parameteters and test the result
?- induce_par([train],P).  % learn the parameteters
?- in(P),test(P,[test],LL,AUCROC,ROC,AUCPR,PR). % test the input theory
*/


:- use_module(library(slipcover)).

:- if(current_predicate(use_rendering/1)).
:- use_rendering(c3).
:- use_rendering(lpad).
:- endif.
:- import between / 3 from basics.

:-sc.

:- set_sc(verbosity,1).
:- set_sc(depth_bound,false).
:- set_sc(neg_ex,given).

bg([]).

fold(train,[train1,train2,train3,train4,train5,train6,train7,train8,train9]).
fold(test,[1]).

output(bought/1).

output(shops/1).

modeh(*,shops(+p)).
modeh(*,bought(+f)).
neg(shops(A,john)):-
  number(A),
  \+ shops(A,john).

neg(shops(A,mary)):-
  number(A),
  \+ shops(A,mary).

neg(bought(A,spaghetti)):-
  number(A),
  \+ shops(A,spaghetti).

neg(bought(A,fish)):-
  number(A),
  \+ shops(A,fish).

neg(bought(A,steak)):-
  number(A),
  \+ shops(A,steak).
in([
(shops(john) : 0.2),
(shops(mary) : 0.9),
(bought(spaghetti ) : 0.5; bought(steak) : 0.5 :- shops(john)),
(bought(spaghetti ) : 0.3; bought(fish) : 0.7:-  shops(mary))]).


begin(model(1)).
bought(fish).
shops(mary).
end(model(1)).