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
fold(test,L):-
  findall(V,between(1,1000,V),L).

output(bought/1).

output(shops/1).

modeh(*,shops(+p)).
modeh(*,bought(+f)).

in([
(shops(john) : 0.2),
(shops(mary) : 0.9),
(bought(spaghetti ) : 0.5; bought(steak) : 0.5 :- shops(john)),
(bought(spaghetti ) : 0.3; bought(fish) : 0.7:-  shops(mary))]).

begin(model(train1)).
neg(bought(fish)).
bought(steak).
bought(spaghetti).
shops(mary).
shops(john).
prob(0.027000).
end_mod(model(train1)).

begin(model(train2)).
bought(fish).
neg(bought(steak)).
bought(spaghetti).
shops(mary).
shops(john).
prob(0.063000).
end_mod(model(train2)).

begin(model(train3)).
neg(bought(fish)).
neg(bought(steak)).
bought(spaghetti).
shops(mary).
shops(john).
prob(0.027000).
end_mod(model(train3)).

begin(model(train4)).
bought(fish).
bought(steak).
neg(bought(spaghetti)).
shops(mary).
shops(john).
prob(0.063000).
end_mod(model(train4)).

begin(model(train5)).
neg(bought(fish)).
neg(bought(steak)).
bought(spaghetti).
neg(shops(mary)).
shops(john).
prob(0.010000).
end_mod(model(train5)).

begin(model(train6)).
neg(bought(fish)).
bought(steak).
neg(bought(spaghetti)).
neg(shops(mary)).
shops(john).
prob(0.010000).
end_mod(model(train6)).

begin(model(train7)).
neg(bought(fish)).
neg(bought(steak)).
bought(spaghetti).
shops(mary).
neg(shops(john)).
prob(0.216000).
end_mod(model(train7)).

begin(model(train8)).
bought(fish).
neg(bought(steak)).
neg(bought(spaghetti)).
shops(mary).
neg(shops(john)).
prob(0.504000).
end_mod(model(train8)).

begin(model(train9)).
neg(bought(fish)).
neg(bought(steak)).
neg(bought(spaghetti)).
neg(shops(mary)).
neg(shops(john)).
prob(0.080000).
end_mod(model(train9)).

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

begin(model(1)).
bought(fish).
shops(mary).
end_mod(model(1)).

begin(model(2)).
bought(spaghetti).
shops(mary).
end_mod(model(2)).

begin(model(3)).
bought(fish).
shops(mary).
end_mod(model(3)).

begin(model(4)).
bought(fish).
shops(mary).
end_mod(model(4)).

begin(model(5)).
bought(fish).
shops(mary).
end_mod(model(5)).

begin(model(6)).
bought(fish).
shops(mary).
end_mod(model(6)).

begin(model(7)).
bought(fish).
shops(mary).
end_mod(model(7)).

begin(model(8)).
bought(fish).
shops(mary).
end_mod(model(8)).

begin(model(9)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(9)).

begin(model(10)).
bought(fish).
shops(mary).
end_mod(model(10)).

begin(model(11)).
bought(fish).
shops(mary).
end_mod(model(11)).

begin(model(12)).
bought(fish).
shops(mary).
end_mod(model(12)).

begin(model(13)).
bought(spaghetti).
shops(mary).
end_mod(model(13)).

begin(model(14)).
bought(fish).
shops(mary).
end_mod(model(14)).

begin(model(15)).
bought(spaghetti).
shops(john).
end_mod(model(15)).

begin(model(16)).
end_mod(model(16)).

begin(model(17)).
bought(fish).
shops(mary).
end_mod(model(17)).

begin(model(18)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(18)).

begin(model(19)).
bought(spaghetti).
shops(mary).
end_mod(model(19)).

begin(model(20)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(20)).

begin(model(21)).
bought(fish).
shops(mary).
end_mod(model(21)).

begin(model(22)).
bought(fish).
shops(mary).
end_mod(model(22)).

begin(model(23)).
bought(fish).
shops(mary).
end_mod(model(23)).

begin(model(24)).
bought(fish).
shops(mary).
end_mod(model(24)).

begin(model(25)).
bought(fish).
shops(mary).
end_mod(model(25)).

begin(model(26)).
end_mod(model(26)).

begin(model(27)).
bought(fish).
shops(mary).
end_mod(model(27)).

begin(model(28)).
bought(fish).
shops(mary).
end_mod(model(28)).

begin(model(29)).
bought(spaghetti).
shops(mary).
end_mod(model(29)).

begin(model(30)).
bought(fish).
shops(mary).
end_mod(model(30)).

begin(model(31)).
bought(fish).
shops(mary).
end_mod(model(31)).

begin(model(32)).
bought(fish).
shops(mary).
end_mod(model(32)).

begin(model(33)).
bought(fish).
shops(mary).
end_mod(model(33)).

begin(model(34)).
bought(spaghetti).
shops(mary).
end_mod(model(34)).

begin(model(35)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(35)).

begin(model(36)).
bought(spaghetti).
shops(mary).
end_mod(model(36)).

begin(model(37)).
bought(fish).
shops(mary).
end_mod(model(37)).

begin(model(38)).
bought(spaghetti).
shops(mary).
end_mod(model(38)).

begin(model(39)).
bought(fish).
shops(mary).
end_mod(model(39)).

begin(model(40)).
bought(fish).
shops(mary).
end_mod(model(40)).

begin(model(41)).
bought(spaghetti).
shops(mary).
end_mod(model(41)).

begin(model(42)).
bought(spaghetti).
shops(mary).
end_mod(model(42)).

begin(model(43)).
bought(spaghetti).
shops(mary).
end_mod(model(43)).

begin(model(44)).
bought(spaghetti).
shops(john).
end_mod(model(44)).

begin(model(45)).
bought(fish).
shops(mary).
end_mod(model(45)).

begin(model(46)).
end_mod(model(46)).

begin(model(47)).
bought(fish).
shops(mary).
end_mod(model(47)).

begin(model(48)).
bought(fish).
shops(mary).
end_mod(model(48)).

begin(model(49)).
bought(fish).
shops(mary).
end_mod(model(49)).

begin(model(50)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(50)).

begin(model(51)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(51)).

begin(model(52)).
bought(fish).
shops(mary).
end_mod(model(52)).

begin(model(53)).
bought(fish).
shops(mary).
end_mod(model(53)).

begin(model(54)).
bought(fish).
shops(mary).
end_mod(model(54)).

begin(model(55)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(55)).

begin(model(56)).
bought(fish).
shops(mary).
end_mod(model(56)).

begin(model(57)).
bought(fish).
shops(mary).
end_mod(model(57)).

begin(model(58)).
bought(spaghetti).
shops(mary).
end_mod(model(58)).

begin(model(59)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(59)).

begin(model(60)).
bought(spaghetti).
shops(mary).
end_mod(model(60)).

begin(model(61)).
bought(spaghetti).
shops(mary).
end_mod(model(61)).

begin(model(62)).
bought(spaghetti).
shops(mary).
end_mod(model(62)).

begin(model(63)).
bought(fish).
shops(mary).
end_mod(model(63)).

begin(model(64)).
bought(spaghetti).
shops(mary).
end_mod(model(64)).

begin(model(65)).
bought(fish).
shops(mary).
end_mod(model(65)).

begin(model(66)).
bought(fish).
shops(mary).
end_mod(model(66)).

begin(model(67)).
bought(fish).
shops(mary).
end_mod(model(67)).

begin(model(68)).
bought(spaghetti).
shops(mary).
end_mod(model(68)).

begin(model(69)).
bought(fish).
shops(mary).
end_mod(model(69)).

begin(model(70)).
bought(fish).
shops(mary).
end_mod(model(70)).

begin(model(71)).
bought(fish).
shops(mary).
end_mod(model(71)).

begin(model(72)).
bought(fish).
shops(mary).
end_mod(model(72)).

begin(model(73)).
bought(fish).
shops(mary).
end_mod(model(73)).

begin(model(74)).
bought(fish).
shops(mary).
end_mod(model(74)).

begin(model(75)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(75)).

begin(model(76)).
bought(steak).
shops(john).
end_mod(model(76)).

begin(model(77)).
bought(fish).
shops(mary).
end_mod(model(77)).

begin(model(78)).
bought(spaghetti).
shops(mary).
end_mod(model(78)).

begin(model(79)).
bought(steak).
shops(john).
end_mod(model(79)).

begin(model(80)).
bought(fish).
shops(mary).
end_mod(model(80)).

begin(model(81)).
bought(spaghetti).
shops(mary).
end_mod(model(81)).

begin(model(82)).
bought(fish).
shops(mary).
end_mod(model(82)).

begin(model(83)).
bought(spaghetti).
shops(mary).
end_mod(model(83)).

begin(model(84)).
bought(fish).
shops(mary).
end_mod(model(84)).

begin(model(85)).
bought(fish).
shops(mary).
end_mod(model(85)).

begin(model(86)).
bought(fish).
shops(mary).
end_mod(model(86)).

begin(model(87)).
bought(spaghetti).
shops(mary).
end_mod(model(87)).

begin(model(88)).
bought(spaghetti).
shops(john).
end_mod(model(88)).

begin(model(89)).
bought(spaghetti).
shops(mary).
end_mod(model(89)).

begin(model(90)).
bought(fish).
shops(mary).
end_mod(model(90)).

begin(model(91)).
bought(spaghetti).
shops(mary).
end_mod(model(91)).

begin(model(92)).
bought(fish).
shops(mary).
end_mod(model(92)).

begin(model(93)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(93)).

begin(model(94)).
bought(spaghetti).
shops(mary).
end_mod(model(94)).

begin(model(95)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(95)).

begin(model(96)).
bought(fish).
shops(mary).
end_mod(model(96)).

begin(model(97)).
bought(fish).
shops(mary).
end_mod(model(97)).

begin(model(98)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(98)).

begin(model(99)).
bought(spaghetti).
shops(mary).
end_mod(model(99)).

begin(model(100)).
bought(spaghetti).
shops(mary).
end_mod(model(100)).

begin(model(101)).
bought(fish).
shops(mary).
end_mod(model(101)).

begin(model(102)).
bought(fish).
shops(mary).
end_mod(model(102)).

begin(model(103)).
bought(fish).
shops(mary).
end_mod(model(103)).

begin(model(104)).
bought(fish).
shops(mary).
end_mod(model(104)).

begin(model(105)).
end_mod(model(105)).

begin(model(106)).
bought(fish).
shops(mary).
end_mod(model(106)).

begin(model(107)).
bought(spaghetti).
shops(mary).
end_mod(model(107)).

begin(model(108)).
bought(fish).
shops(mary).
end_mod(model(108)).

begin(model(109)).
bought(fish).
shops(mary).
end_mod(model(109)).

begin(model(110)).
bought(fish).
shops(mary).
end_mod(model(110)).

begin(model(111)).
bought(fish).
shops(mary).
end_mod(model(111)).

begin(model(112)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(112)).

begin(model(113)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(113)).

begin(model(114)).
bought(fish).
shops(mary).
end_mod(model(114)).

begin(model(115)).
bought(spaghetti).
shops(mary).
end_mod(model(115)).

begin(model(116)).
bought(fish).
shops(mary).
end_mod(model(116)).

begin(model(117)).
bought(spaghetti).
shops(mary).
end_mod(model(117)).

begin(model(118)).
bought(fish).
shops(mary).
end_mod(model(118)).

begin(model(119)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(119)).

begin(model(120)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(120)).

begin(model(121)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(121)).

begin(model(122)).
bought(fish).
shops(mary).
end_mod(model(122)).

begin(model(123)).
end_mod(model(123)).

begin(model(124)).
bought(fish).
shops(mary).
end_mod(model(124)).

begin(model(125)).
bought(fish).
shops(mary).
end_mod(model(125)).

begin(model(126)).
bought(spaghetti).
shops(mary).
end_mod(model(126)).

begin(model(127)).
bought(fish).
shops(mary).
end_mod(model(127)).

begin(model(128)).
bought(fish).
shops(mary).
end_mod(model(128)).

begin(model(129)).
bought(fish).
shops(mary).
end_mod(model(129)).

begin(model(130)).
bought(fish).
shops(mary).
end_mod(model(130)).

begin(model(131)).
bought(fish).
shops(mary).
end_mod(model(131)).

begin(model(132)).
bought(fish).
shops(mary).
end_mod(model(132)).

begin(model(133)).
bought(fish).
shops(mary).
end_mod(model(133)).

begin(model(134)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(134)).

begin(model(135)).
bought(fish).
shops(mary).
end_mod(model(135)).

begin(model(136)).
bought(fish).
shops(mary).
end_mod(model(136)).

begin(model(137)).
bought(fish).
shops(mary).
end_mod(model(137)).

begin(model(138)).
bought(fish).
shops(mary).
end_mod(model(138)).

begin(model(139)).
bought(fish).
shops(mary).
end_mod(model(139)).

begin(model(140)).
bought(fish).
shops(mary).
end_mod(model(140)).

begin(model(141)).
end_mod(model(141)).

begin(model(142)).
bought(spaghetti).
shops(mary).
end_mod(model(142)).

begin(model(143)).
bought(spaghetti).
shops(mary).
end_mod(model(143)).

begin(model(144)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(144)).

begin(model(145)).
bought(spaghetti).
shops(mary).
end_mod(model(145)).

begin(model(146)).
end_mod(model(146)).

begin(model(147)).
bought(spaghetti).
shops(mary).
end_mod(model(147)).

begin(model(148)).
bought(fish).
shops(mary).
end_mod(model(148)).

begin(model(149)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(149)).

begin(model(150)).
end_mod(model(150)).

begin(model(151)).
bought(steak).
shops(john).
end_mod(model(151)).

begin(model(152)).
bought(spaghetti).
shops(mary).
end_mod(model(152)).

begin(model(153)).
bought(fish).
shops(mary).
end_mod(model(153)).

begin(model(154)).
bought(fish).
shops(mary).
end_mod(model(154)).

begin(model(155)).
bought(fish).
shops(mary).
end_mod(model(155)).

begin(model(156)).
bought(fish).
shops(mary).
end_mod(model(156)).

begin(model(157)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(157)).

begin(model(158)).
bought(spaghetti).
shops(mary).
end_mod(model(158)).

begin(model(159)).
bought(fish).
shops(mary).
end_mod(model(159)).

begin(model(160)).
bought(spaghetti).
shops(mary).
end_mod(model(160)).

begin(model(161)).
bought(fish).
shops(mary).
end_mod(model(161)).

begin(model(162)).
bought(fish).
shops(mary).
end_mod(model(162)).

begin(model(163)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(163)).

begin(model(164)).
bought(fish).
shops(mary).
end_mod(model(164)).

begin(model(165)).
bought(spaghetti).
shops(mary).
end_mod(model(165)).

begin(model(166)).
bought(spaghetti).
shops(mary).
end_mod(model(166)).

begin(model(167)).
bought(fish).
shops(mary).
end_mod(model(167)).

begin(model(168)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(168)).

begin(model(169)).
bought(fish).
shops(mary).
end_mod(model(169)).

begin(model(170)).
bought(spaghetti).
shops(mary).
end_mod(model(170)).

begin(model(171)).
bought(spaghetti).
shops(mary).
end_mod(model(171)).

begin(model(172)).
bought(fish).
shops(mary).
end_mod(model(172)).

begin(model(173)).
bought(fish).
shops(mary).
end_mod(model(173)).

begin(model(174)).
bought(fish).
shops(mary).
end_mod(model(174)).

begin(model(175)).
bought(fish).
shops(mary).
end_mod(model(175)).

begin(model(176)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(176)).

begin(model(177)).
bought(fish).
shops(mary).
end_mod(model(177)).

begin(model(178)).
bought(spaghetti).
shops(mary).
end_mod(model(178)).

begin(model(179)).
bought(fish).
shops(mary).
end_mod(model(179)).

begin(model(180)).
bought(fish).
shops(mary).
end_mod(model(180)).

begin(model(181)).
bought(fish).
shops(mary).
end_mod(model(181)).

begin(model(182)).
bought(fish).
shops(mary).
end_mod(model(182)).

begin(model(183)).
bought(fish).
shops(mary).
end_mod(model(183)).

begin(model(184)).
bought(spaghetti).
shops(mary).
end_mod(model(184)).

begin(model(185)).
bought(fish).
shops(mary).
end_mod(model(185)).

begin(model(186)).
bought(spaghetti).
shops(mary).
end_mod(model(186)).

begin(model(187)).
bought(fish).
shops(mary).
end_mod(model(187)).

begin(model(188)).
bought(spaghetti).
shops(mary).
end_mod(model(188)).

begin(model(189)).
bought(spaghetti).
shops(mary).
end_mod(model(189)).

begin(model(190)).
bought(spaghetti).
shops(mary).
end_mod(model(190)).

begin(model(191)).
end_mod(model(191)).

begin(model(192)).
bought(spaghetti).
shops(mary).
end_mod(model(192)).

begin(model(193)).
bought(fish).
shops(mary).
end_mod(model(193)).

begin(model(194)).
bought(fish).
shops(mary).
end_mod(model(194)).

begin(model(195)).
bought(fish).
shops(mary).
end_mod(model(195)).

begin(model(196)).
bought(fish).
shops(mary).
end_mod(model(196)).

begin(model(197)).
bought(fish).
shops(mary).
end_mod(model(197)).

begin(model(198)).
bought(fish).
shops(mary).
end_mod(model(198)).

begin(model(199)).
bought(fish).
shops(mary).
end_mod(model(199)).

begin(model(200)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(200)).

begin(model(201)).
bought(fish).
shops(mary).
end_mod(model(201)).

begin(model(202)).
bought(fish).
shops(mary).
end_mod(model(202)).

begin(model(203)).
bought(fish).
shops(mary).
end_mod(model(203)).

begin(model(204)).
bought(fish).
shops(mary).
end_mod(model(204)).

begin(model(205)).
bought(fish).
shops(mary).
end_mod(model(205)).

begin(model(206)).
bought(fish).
shops(mary).
end_mod(model(206)).

begin(model(207)).
bought(spaghetti).
shops(mary).
end_mod(model(207)).

begin(model(208)).
bought(fish).
shops(mary).
end_mod(model(208)).

begin(model(209)).
end_mod(model(209)).

begin(model(210)).
bought(fish).
shops(mary).
end_mod(model(210)).

begin(model(211)).
bought(fish).
shops(mary).
end_mod(model(211)).

begin(model(212)).
bought(spaghetti).
shops(mary).
end_mod(model(212)).

begin(model(213)).
bought(spaghetti).
shops(mary).
end_mod(model(213)).

begin(model(214)).
bought(fish).
shops(mary).
end_mod(model(214)).

begin(model(215)).
bought(fish).
shops(mary).
end_mod(model(215)).

begin(model(216)).
bought(fish).
shops(mary).
end_mod(model(216)).

begin(model(217)).
bought(fish).
shops(mary).
end_mod(model(217)).

begin(model(218)).
bought(spaghetti).
shops(mary).
end_mod(model(218)).

begin(model(219)).
bought(spaghetti).
shops(mary).
end_mod(model(219)).

begin(model(220)).
bought(fish).
shops(mary).
end_mod(model(220)).

begin(model(221)).
bought(fish).
shops(mary).
end_mod(model(221)).

begin(model(222)).
bought(spaghetti).
shops(mary).
end_mod(model(222)).

begin(model(223)).
bought(fish).
shops(mary).
end_mod(model(223)).

begin(model(224)).
end_mod(model(224)).

begin(model(225)).
end_mod(model(225)).

begin(model(226)).
bought(fish).
shops(mary).
end_mod(model(226)).

begin(model(227)).
bought(fish).
shops(mary).
end_mod(model(227)).

begin(model(228)).
bought(fish).
shops(mary).
end_mod(model(228)).

begin(model(229)).
bought(fish).
shops(mary).
end_mod(model(229)).

begin(model(230)).
bought(spaghetti).
shops(mary).
end_mod(model(230)).

begin(model(231)).
bought(spaghetti).
shops(mary).
end_mod(model(231)).

begin(model(232)).
bought(fish).
shops(mary).
end_mod(model(232)).

begin(model(233)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(233)).

begin(model(234)).
bought(spaghetti).
shops(mary).
end_mod(model(234)).

begin(model(235)).
bought(fish).
shops(mary).
end_mod(model(235)).

begin(model(236)).
bought(fish).
shops(mary).
end_mod(model(236)).

begin(model(237)).
bought(fish).
shops(mary).
end_mod(model(237)).

begin(model(238)).
bought(fish).
shops(mary).
end_mod(model(238)).

begin(model(239)).
bought(fish).
shops(mary).
end_mod(model(239)).

begin(model(240)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(240)).

begin(model(241)).
bought(fish).
shops(mary).
end_mod(model(241)).

begin(model(242)).
bought(spaghetti).
shops(mary).
end_mod(model(242)).

begin(model(243)).
bought(fish).
shops(mary).
end_mod(model(243)).

begin(model(244)).
bought(spaghetti).
shops(mary).
end_mod(model(244)).

begin(model(245)).
bought(fish).
shops(mary).
end_mod(model(245)).

begin(model(246)).
bought(fish).
shops(mary).
end_mod(model(246)).

begin(model(247)).
bought(fish).
shops(mary).
end_mod(model(247)).

begin(model(248)).
bought(spaghetti).
shops(mary).
end_mod(model(248)).

begin(model(249)).
bought(fish).
shops(mary).
end_mod(model(249)).

begin(model(250)).
bought(spaghetti).
shops(mary).
end_mod(model(250)).

begin(model(251)).
bought(spaghetti).
shops(mary).
end_mod(model(251)).

begin(model(252)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(252)).

begin(model(253)).
bought(fish).
shops(mary).
end_mod(model(253)).

begin(model(254)).
bought(fish).
shops(mary).
end_mod(model(254)).

begin(model(255)).
bought(fish).
shops(mary).
end_mod(model(255)).

begin(model(256)).
bought(fish).
shops(mary).
end_mod(model(256)).

begin(model(257)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(257)).

begin(model(258)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(258)).

begin(model(259)).
bought(spaghetti).
shops(mary).
end_mod(model(259)).

begin(model(260)).
bought(fish).
shops(mary).
end_mod(model(260)).

begin(model(261)).
bought(fish).
shops(mary).
end_mod(model(261)).

begin(model(262)).
bought(fish).
shops(mary).
end_mod(model(262)).

begin(model(263)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(263)).

begin(model(264)).
end_mod(model(264)).

begin(model(265)).
end_mod(model(265)).

begin(model(266)).
bought(spaghetti).
shops(mary).
end_mod(model(266)).

begin(model(267)).
bought(fish).
shops(mary).
end_mod(model(267)).

begin(model(268)).
bought(fish).
shops(mary).
end_mod(model(268)).

begin(model(269)).
bought(fish).
shops(mary).
end_mod(model(269)).

begin(model(270)).
bought(spaghetti).
shops(mary).
end_mod(model(270)).

begin(model(271)).
bought(fish).
shops(mary).
end_mod(model(271)).

begin(model(272)).
bought(fish).
shops(mary).
end_mod(model(272)).

begin(model(273)).
bought(fish).
shops(mary).
end_mod(model(273)).

begin(model(274)).
bought(fish).
shops(mary).
end_mod(model(274)).

begin(model(275)).
bought(fish).
shops(mary).
end_mod(model(275)).

begin(model(276)).
bought(steak).
shops(john).
end_mod(model(276)).

begin(model(277)).
bought(spaghetti).
shops(mary).
end_mod(model(277)).

begin(model(278)).
bought(fish).
shops(mary).
end_mod(model(278)).

begin(model(279)).
bought(fish).
shops(mary).
end_mod(model(279)).

begin(model(280)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(280)).

begin(model(281)).
bought(fish).
shops(mary).
end_mod(model(281)).

begin(model(282)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(282)).

begin(model(283)).
bought(spaghetti).
shops(mary).
end_mod(model(283)).

begin(model(284)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(284)).

begin(model(285)).
bought(fish).
shops(mary).
end_mod(model(285)).

begin(model(286)).
bought(fish).
shops(mary).
end_mod(model(286)).

begin(model(287)).
bought(fish).
shops(mary).
end_mod(model(287)).

begin(model(288)).
bought(fish).
shops(mary).
end_mod(model(288)).

begin(model(289)).
bought(fish).
shops(mary).
end_mod(model(289)).

begin(model(290)).
bought(fish).
shops(mary).
end_mod(model(290)).

begin(model(291)).
end_mod(model(291)).

begin(model(292)).
bought(fish).
shops(mary).
end_mod(model(292)).

begin(model(293)).
bought(fish).
shops(mary).
end_mod(model(293)).

begin(model(294)).
bought(steak).
shops(john).
end_mod(model(294)).

begin(model(295)).
bought(fish).
shops(mary).
end_mod(model(295)).

begin(model(296)).
bought(spaghetti).
shops(mary).
end_mod(model(296)).

begin(model(297)).
bought(fish).
shops(mary).
end_mod(model(297)).

begin(model(298)).
bought(spaghetti).
shops(mary).
end_mod(model(298)).

begin(model(299)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(299)).

begin(model(300)).
bought(fish).
shops(mary).
end_mod(model(300)).

begin(model(301)).
bought(fish).
shops(mary).
end_mod(model(301)).

begin(model(302)).
bought(fish).
shops(mary).
end_mod(model(302)).

begin(model(303)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(303)).

begin(model(304)).
bought(fish).
shops(mary).
end_mod(model(304)).

begin(model(305)).
bought(fish).
shops(mary).
end_mod(model(305)).

begin(model(306)).
bought(fish).
shops(mary).
end_mod(model(306)).

begin(model(307)).
bought(fish).
shops(mary).
end_mod(model(307)).

begin(model(308)).
bought(fish).
shops(mary).
end_mod(model(308)).

begin(model(309)).
bought(fish).
shops(mary).
end_mod(model(309)).

begin(model(310)).
bought(fish).
shops(mary).
end_mod(model(310)).

begin(model(311)).
bought(spaghetti).
shops(mary).
end_mod(model(311)).

begin(model(312)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(312)).

begin(model(313)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(313)).

begin(model(314)).
bought(spaghetti).
shops(john).
end_mod(model(314)).

begin(model(315)).
bought(fish).
shops(mary).
end_mod(model(315)).

begin(model(316)).
bought(fish).
shops(mary).
end_mod(model(316)).

begin(model(317)).
bought(spaghetti).
shops(mary).
end_mod(model(317)).

begin(model(318)).
bought(spaghetti).
shops(mary).
end_mod(model(318)).

begin(model(319)).
bought(spaghetti).
shops(mary).
end_mod(model(319)).

begin(model(320)).
bought(fish).
shops(mary).
end_mod(model(320)).

begin(model(321)).
bought(fish).
shops(mary).
end_mod(model(321)).

begin(model(322)).
bought(fish).
shops(mary).
end_mod(model(322)).

begin(model(323)).
bought(fish).
shops(mary).
end_mod(model(323)).

begin(model(324)).
bought(fish).
shops(mary).
end_mod(model(324)).

begin(model(325)).
bought(fish).
shops(mary).
end_mod(model(325)).

begin(model(326)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(326)).

begin(model(327)).
bought(fish).
shops(mary).
end_mod(model(327)).

begin(model(328)).
bought(spaghetti).
shops(mary).
end_mod(model(328)).

begin(model(329)).
bought(fish).
shops(mary).
end_mod(model(329)).

begin(model(330)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(330)).

begin(model(331)).
bought(spaghetti).
shops(mary).
end_mod(model(331)).

begin(model(332)).
bought(fish).
shops(mary).
end_mod(model(332)).

begin(model(333)).
bought(spaghetti).
shops(mary).
end_mod(model(333)).

begin(model(334)).
bought(spaghetti).
shops(mary).
end_mod(model(334)).

begin(model(335)).
bought(fish).
shops(mary).
end_mod(model(335)).

begin(model(336)).
bought(spaghetti).
shops(mary).
end_mod(model(336)).

begin(model(337)).
bought(fish).
shops(mary).
end_mod(model(337)).

begin(model(338)).
bought(fish).
shops(mary).
end_mod(model(338)).

begin(model(339)).
bought(spaghetti).
shops(mary).
end_mod(model(339)).

begin(model(340)).
bought(fish).
shops(mary).
end_mod(model(340)).

begin(model(341)).
bought(fish).
shops(mary).
end_mod(model(341)).

begin(model(342)).
bought(fish).
shops(mary).
end_mod(model(342)).

begin(model(343)).
bought(fish).
shops(mary).
end_mod(model(343)).

begin(model(344)).
bought(fish).
shops(mary).
end_mod(model(344)).

begin(model(345)).
bought(fish).
shops(mary).
end_mod(model(345)).

begin(model(346)).
bought(fish).
shops(mary).
end_mod(model(346)).

begin(model(347)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(347)).

begin(model(348)).
bought(fish).
shops(mary).
end_mod(model(348)).

begin(model(349)).
bought(fish).
shops(mary).
end_mod(model(349)).

begin(model(350)).
bought(fish).
shops(mary).
end_mod(model(350)).

begin(model(351)).
bought(spaghetti).
shops(mary).
end_mod(model(351)).

begin(model(352)).
bought(fish).
shops(mary).
end_mod(model(352)).

begin(model(353)).
bought(fish).
shops(mary).
end_mod(model(353)).

begin(model(354)).
bought(fish).
shops(mary).
end_mod(model(354)).

begin(model(355)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(355)).

begin(model(356)).
bought(fish).
shops(mary).
end_mod(model(356)).

begin(model(357)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(357)).

begin(model(358)).
bought(fish).
shops(mary).
end_mod(model(358)).

begin(model(359)).
bought(fish).
shops(mary).
end_mod(model(359)).

begin(model(360)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(360)).

begin(model(361)).
bought(spaghetti).
shops(mary).
end_mod(model(361)).

begin(model(362)).
bought(spaghetti).
shops(mary).
end_mod(model(362)).

begin(model(363)).
bought(fish).
shops(mary).
end_mod(model(363)).

begin(model(364)).
bought(spaghetti).
shops(mary).
end_mod(model(364)).

begin(model(365)).
bought(fish).
shops(mary).
end_mod(model(365)).

begin(model(366)).
bought(fish).
shops(mary).
end_mod(model(366)).

begin(model(367)).
bought(fish).
shops(mary).
end_mod(model(367)).

begin(model(368)).
bought(fish).
shops(mary).
end_mod(model(368)).

begin(model(369)).
end_mod(model(369)).

begin(model(370)).
bought(fish).
shops(mary).
end_mod(model(370)).

begin(model(371)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(371)).

begin(model(372)).
bought(fish).
shops(mary).
end_mod(model(372)).

begin(model(373)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(373)).

begin(model(374)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(374)).

begin(model(375)).
bought(fish).
shops(mary).
end_mod(model(375)).

begin(model(376)).
bought(fish).
shops(mary).
end_mod(model(376)).

begin(model(377)).
bought(fish).
shops(mary).
end_mod(model(377)).

begin(model(378)).
bought(fish).
shops(mary).
end_mod(model(378)).

begin(model(379)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(379)).

begin(model(380)).
bought(spaghetti).
shops(mary).
end_mod(model(380)).

begin(model(381)).
bought(fish).
shops(mary).
end_mod(model(381)).

begin(model(382)).
bought(fish).
shops(mary).
end_mod(model(382)).

begin(model(383)).
bought(fish).
shops(mary).
end_mod(model(383)).

begin(model(384)).
bought(fish).
shops(mary).
end_mod(model(384)).

begin(model(385)).
bought(fish).
shops(mary).
end_mod(model(385)).

begin(model(386)).
bought(fish).
shops(mary).
end_mod(model(386)).

begin(model(387)).
bought(spaghetti).
shops(john).
end_mod(model(387)).

begin(model(388)).
bought(fish).
shops(mary).
end_mod(model(388)).

begin(model(389)).
bought(fish).
shops(mary).
end_mod(model(389)).

begin(model(390)).
bought(fish).
shops(mary).
end_mod(model(390)).

begin(model(391)).
bought(spaghetti).
shops(mary).
end_mod(model(391)).

begin(model(392)).
bought(spaghetti).
shops(john).
end_mod(model(392)).

begin(model(393)).
bought(fish).
shops(mary).
end_mod(model(393)).

begin(model(394)).
bought(fish).
shops(mary).
end_mod(model(394)).

begin(model(395)).
bought(spaghetti).
shops(mary).
end_mod(model(395)).

begin(model(396)).
bought(fish).
shops(mary).
end_mod(model(396)).

begin(model(397)).
bought(fish).
shops(mary).
end_mod(model(397)).

begin(model(398)).
bought(fish).
shops(mary).
end_mod(model(398)).

begin(model(399)).
bought(spaghetti).
shops(mary).
end_mod(model(399)).

begin(model(400)).
bought(fish).
shops(mary).
end_mod(model(400)).

begin(model(401)).
bought(fish).
shops(mary).
end_mod(model(401)).

begin(model(402)).
bought(spaghetti).
shops(mary).
end_mod(model(402)).

begin(model(403)).
bought(fish).
shops(mary).
end_mod(model(403)).

begin(model(404)).
bought(fish).
shops(mary).
end_mod(model(404)).

begin(model(405)).
bought(spaghetti).
shops(mary).
end_mod(model(405)).

begin(model(406)).
bought(fish).
shops(mary).
end_mod(model(406)).

begin(model(407)).
bought(fish).
shops(mary).
end_mod(model(407)).

begin(model(408)).
bought(fish).
shops(mary).
end_mod(model(408)).

begin(model(409)).
bought(steak).
shops(john).
end_mod(model(409)).

begin(model(410)).
bought(fish).
shops(mary).
end_mod(model(410)).

begin(model(411)).
bought(fish).
shops(mary).
end_mod(model(411)).

begin(model(412)).
bought(fish).
shops(mary).
end_mod(model(412)).

begin(model(413)).
bought(fish).
shops(mary).
end_mod(model(413)).

begin(model(414)).
bought(fish).
shops(mary).
end_mod(model(414)).

begin(model(415)).
bought(fish).
shops(mary).
end_mod(model(415)).

begin(model(416)).
end_mod(model(416)).

begin(model(417)).
bought(spaghetti).
shops(mary).
end_mod(model(417)).

begin(model(418)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(418)).

begin(model(419)).
bought(spaghetti).
shops(mary).
end_mod(model(419)).

begin(model(420)).
bought(spaghetti).
shops(mary).
end_mod(model(420)).

begin(model(421)).
bought(fish).
shops(mary).
end_mod(model(421)).

begin(model(422)).
bought(spaghetti).
shops(mary).
end_mod(model(422)).

begin(model(423)).
bought(fish).
shops(mary).
end_mod(model(423)).

begin(model(424)).
bought(fish).
shops(mary).
end_mod(model(424)).

begin(model(425)).
bought(fish).
shops(mary).
end_mod(model(425)).

begin(model(426)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(426)).

begin(model(427)).
bought(spaghetti).
shops(mary).
end_mod(model(427)).

begin(model(428)).
bought(fish).
shops(mary).
end_mod(model(428)).

begin(model(429)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(429)).

begin(model(430)).
bought(spaghetti).
shops(mary).
end_mod(model(430)).

begin(model(431)).
bought(fish).
shops(mary).
end_mod(model(431)).

begin(model(432)).
bought(fish).
shops(mary).
end_mod(model(432)).

begin(model(433)).
bought(fish).
shops(mary).
end_mod(model(433)).

begin(model(434)).
bought(fish).
shops(mary).
end_mod(model(434)).

begin(model(435)).
bought(fish).
shops(mary).
end_mod(model(435)).

begin(model(436)).
bought(fish).
shops(mary).
end_mod(model(436)).

begin(model(437)).
bought(fish).
shops(mary).
end_mod(model(437)).

begin(model(438)).
bought(fish).
shops(mary).
end_mod(model(438)).

begin(model(439)).
bought(fish).
shops(mary).
end_mod(model(439)).

begin(model(440)).
bought(spaghetti).
shops(mary).
end_mod(model(440)).

begin(model(441)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(441)).

begin(model(442)).
bought(fish).
shops(mary).
end_mod(model(442)).

begin(model(443)).
bought(spaghetti).
shops(mary).
end_mod(model(443)).

begin(model(444)).
bought(fish).
shops(mary).
end_mod(model(444)).

begin(model(445)).
bought(fish).
shops(mary).
end_mod(model(445)).

begin(model(446)).
bought(spaghetti).
shops(mary).
end_mod(model(446)).

begin(model(447)).
bought(fish).
shops(mary).
end_mod(model(447)).

begin(model(448)).
bought(fish).
shops(mary).
end_mod(model(448)).

begin(model(449)).
bought(fish).
shops(mary).
end_mod(model(449)).

begin(model(450)).
bought(fish).
shops(mary).
end_mod(model(450)).

begin(model(451)).
bought(fish).
shops(mary).
end_mod(model(451)).

begin(model(452)).
bought(spaghetti).
shops(mary).
end_mod(model(452)).

begin(model(453)).
bought(fish).
shops(mary).
end_mod(model(453)).

begin(model(454)).
bought(fish).
shops(mary).
end_mod(model(454)).

begin(model(455)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(455)).

begin(model(456)).
bought(spaghetti).
shops(mary).
end_mod(model(456)).

begin(model(457)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(457)).

begin(model(458)).
bought(fish).
shops(mary).
end_mod(model(458)).

begin(model(459)).
bought(fish).
shops(mary).
end_mod(model(459)).

begin(model(460)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(460)).

begin(model(461)).
bought(spaghetti).
shops(mary).
end_mod(model(461)).

begin(model(462)).
bought(fish).
shops(mary).
end_mod(model(462)).

begin(model(463)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(463)).

begin(model(464)).
bought(spaghetti).
shops(mary).
end_mod(model(464)).

begin(model(465)).
bought(fish).
shops(mary).
end_mod(model(465)).

begin(model(466)).
bought(fish).
shops(mary).
end_mod(model(466)).

begin(model(467)).
bought(fish).
shops(mary).
end_mod(model(467)).

begin(model(468)).
bought(steak).
shops(john).
end_mod(model(468)).

begin(model(469)).
bought(steak).
shops(john).
end_mod(model(469)).

begin(model(470)).
bought(fish).
shops(mary).
end_mod(model(470)).

begin(model(471)).
bought(fish).
shops(mary).
end_mod(model(471)).

begin(model(472)).
bought(fish).
shops(mary).
end_mod(model(472)).

begin(model(473)).
bought(fish).
shops(mary).
end_mod(model(473)).

begin(model(474)).
bought(spaghetti).
shops(mary).
end_mod(model(474)).

begin(model(475)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(475)).

begin(model(476)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(476)).

begin(model(477)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(477)).

begin(model(478)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(478)).

begin(model(479)).
bought(spaghetti).
shops(mary).
end_mod(model(479)).

begin(model(480)).
bought(fish).
shops(mary).
end_mod(model(480)).

begin(model(481)).
bought(fish).
shops(mary).
end_mod(model(481)).

begin(model(482)).
bought(fish).
shops(mary).
end_mod(model(482)).

begin(model(483)).
bought(fish).
shops(mary).
end_mod(model(483)).

begin(model(484)).
bought(fish).
shops(mary).
end_mod(model(484)).

begin(model(485)).
bought(spaghetti).
shops(mary).
end_mod(model(485)).

begin(model(486)).
bought(spaghetti).
shops(mary).
end_mod(model(486)).

begin(model(487)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(487)).

begin(model(488)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(488)).

begin(model(489)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(489)).

begin(model(490)).
bought(fish).
shops(mary).
end_mod(model(490)).

begin(model(491)).
bought(spaghetti).
shops(mary).
end_mod(model(491)).

begin(model(492)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(492)).

begin(model(493)).
bought(fish).
shops(mary).
end_mod(model(493)).

begin(model(494)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(494)).

begin(model(495)).
bought(spaghetti).
shops(mary).
end_mod(model(495)).

begin(model(496)).
bought(spaghetti).
shops(mary).
end_mod(model(496)).

begin(model(497)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(497)).

begin(model(498)).
bought(fish).
shops(mary).
end_mod(model(498)).

begin(model(499)).
bought(fish).
shops(mary).
end_mod(model(499)).

begin(model(500)).
bought(fish).
shops(mary).
end_mod(model(500)).

begin(model(501)).
bought(spaghetti).
shops(mary).
end_mod(model(501)).

begin(model(502)).
bought(fish).
shops(mary).
end_mod(model(502)).

begin(model(503)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(503)).

begin(model(504)).
bought(fish).
shops(mary).
end_mod(model(504)).

begin(model(505)).
bought(spaghetti).
shops(mary).
end_mod(model(505)).

begin(model(506)).
bought(fish).
shops(mary).
end_mod(model(506)).

begin(model(507)).
bought(fish).
shops(mary).
end_mod(model(507)).

begin(model(508)).
bought(spaghetti).
shops(mary).
end_mod(model(508)).

begin(model(509)).
bought(fish).
shops(mary).
end_mod(model(509)).

begin(model(510)).
bought(fish).
shops(mary).
end_mod(model(510)).

begin(model(511)).
bought(spaghetti).
shops(mary).
end_mod(model(511)).

begin(model(512)).
bought(fish).
shops(mary).
end_mod(model(512)).

begin(model(513)).
bought(fish).
shops(mary).
end_mod(model(513)).

begin(model(514)).
bought(fish).
shops(mary).
end_mod(model(514)).

begin(model(515)).
bought(spaghetti).
shops(mary).
end_mod(model(515)).

begin(model(516)).
bought(spaghetti).
shops(john).
end_mod(model(516)).

begin(model(517)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(517)).

begin(model(518)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(518)).

begin(model(519)).
bought(fish).
shops(mary).
end_mod(model(519)).

begin(model(520)).
bought(fish).
shops(mary).
end_mod(model(520)).

begin(model(521)).
bought(fish).
shops(mary).
end_mod(model(521)).

begin(model(522)).
bought(fish).
shops(mary).
end_mod(model(522)).

begin(model(523)).
bought(fish).
shops(mary).
end_mod(model(523)).

begin(model(524)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(524)).

begin(model(525)).
bought(spaghetti).
shops(mary).
end_mod(model(525)).

begin(model(526)).
bought(fish).
shops(mary).
end_mod(model(526)).

begin(model(527)).
bought(fish).
shops(mary).
end_mod(model(527)).

begin(model(528)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(528)).

begin(model(529)).
bought(spaghetti).
shops(mary).
end_mod(model(529)).

begin(model(530)).
bought(spaghetti).
shops(mary).
end_mod(model(530)).

begin(model(531)).
bought(fish).
shops(mary).
end_mod(model(531)).

begin(model(532)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(532)).

begin(model(533)).
end_mod(model(533)).

begin(model(534)).
bought(fish).
shops(mary).
end_mod(model(534)).

begin(model(535)).
bought(spaghetti).
shops(mary).
end_mod(model(535)).

begin(model(536)).
bought(spaghetti).
shops(mary).
end_mod(model(536)).

begin(model(537)).
bought(fish).
shops(mary).
end_mod(model(537)).

begin(model(538)).
bought(fish).
shops(mary).
end_mod(model(538)).

begin(model(539)).
bought(fish).
shops(mary).
end_mod(model(539)).

begin(model(540)).
bought(fish).
shops(mary).
end_mod(model(540)).

begin(model(541)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(541)).

begin(model(542)).
bought(fish).
shops(mary).
end_mod(model(542)).

begin(model(543)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(543)).

begin(model(544)).
bought(spaghetti).
shops(mary).
end_mod(model(544)).

begin(model(545)).
bought(fish).
shops(mary).
end_mod(model(545)).

begin(model(546)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(546)).

begin(model(547)).
bought(fish).
shops(mary).
end_mod(model(547)).

begin(model(548)).
bought(spaghetti).
shops(mary).
end_mod(model(548)).

begin(model(549)).
bought(fish).
shops(mary).
end_mod(model(549)).

begin(model(550)).
bought(fish).
shops(mary).
end_mod(model(550)).

begin(model(551)).
bought(fish).
shops(mary).
end_mod(model(551)).

begin(model(552)).
bought(spaghetti).
shops(mary).
end_mod(model(552)).

begin(model(553)).
bought(spaghetti).
shops(mary).
end_mod(model(553)).

begin(model(554)).
bought(fish).
shops(mary).
end_mod(model(554)).

begin(model(555)).
bought(fish).
shops(mary).
end_mod(model(555)).

begin(model(556)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(556)).

begin(model(557)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(557)).

begin(model(558)).
bought(fish).
shops(mary).
end_mod(model(558)).

begin(model(559)).
bought(spaghetti).
shops(mary).
end_mod(model(559)).

begin(model(560)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(560)).

begin(model(561)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(561)).

begin(model(562)).
bought(spaghetti).
shops(mary).
end_mod(model(562)).

begin(model(563)).
bought(spaghetti).
shops(mary).
end_mod(model(563)).

begin(model(564)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(564)).

begin(model(565)).
bought(fish).
shops(mary).
end_mod(model(565)).

begin(model(566)).
bought(spaghetti).
shops(mary).
end_mod(model(566)).

begin(model(567)).
bought(spaghetti).
shops(mary).
end_mod(model(567)).

begin(model(568)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(568)).

begin(model(569)).
bought(fish).
shops(mary).
end_mod(model(569)).

begin(model(570)).
bought(fish).
shops(mary).
end_mod(model(570)).

begin(model(571)).
bought(fish).
shops(mary).
end_mod(model(571)).

begin(model(572)).
bought(fish).
shops(mary).
end_mod(model(572)).

begin(model(573)).
bought(fish).
shops(mary).
end_mod(model(573)).

begin(model(574)).
bought(fish).
shops(mary).
end_mod(model(574)).

begin(model(575)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(575)).

begin(model(576)).
bought(fish).
shops(mary).
end_mod(model(576)).

begin(model(577)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(577)).

begin(model(578)).
bought(fish).
shops(mary).
end_mod(model(578)).

begin(model(579)).
bought(spaghetti).
shops(mary).
end_mod(model(579)).

begin(model(580)).
bought(fish).
shops(mary).
end_mod(model(580)).

begin(model(581)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(581)).

begin(model(582)).
bought(spaghetti).
shops(mary).
end_mod(model(582)).

begin(model(583)).
bought(fish).
shops(mary).
end_mod(model(583)).

begin(model(584)).
bought(fish).
shops(mary).
end_mod(model(584)).

begin(model(585)).
bought(fish).
shops(mary).
end_mod(model(585)).

begin(model(586)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(586)).

begin(model(587)).
bought(fish).
shops(mary).
end_mod(model(587)).

begin(model(588)).
bought(fish).
shops(mary).
end_mod(model(588)).

begin(model(589)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(589)).

begin(model(590)).
bought(fish).
shops(mary).
end_mod(model(590)).

begin(model(591)).
bought(fish).
shops(mary).
end_mod(model(591)).

begin(model(592)).
bought(spaghetti).
shops(mary).
end_mod(model(592)).

begin(model(593)).
bought(spaghetti).
shops(mary).
end_mod(model(593)).

begin(model(594)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(594)).

begin(model(595)).
bought(fish).
shops(mary).
end_mod(model(595)).

begin(model(596)).
bought(spaghetti).
shops(mary).
end_mod(model(596)).

begin(model(597)).
bought(spaghetti).
shops(mary).
end_mod(model(597)).

begin(model(598)).
end_mod(model(598)).

begin(model(599)).
bought(fish).
shops(mary).
end_mod(model(599)).

begin(model(600)).
bought(fish).
shops(mary).
end_mod(model(600)).

begin(model(601)).
bought(spaghetti).
shops(mary).
end_mod(model(601)).

begin(model(602)).
bought(fish).
shops(mary).
end_mod(model(602)).

begin(model(603)).
bought(fish).
shops(mary).
end_mod(model(603)).

begin(model(604)).
bought(fish).
shops(mary).
end_mod(model(604)).

begin(model(605)).
bought(fish).
shops(mary).
end_mod(model(605)).

begin(model(606)).
end_mod(model(606)).

begin(model(607)).
bought(spaghetti).
shops(mary).
end_mod(model(607)).

begin(model(608)).
bought(spaghetti).
shops(mary).
end_mod(model(608)).

begin(model(609)).
bought(fish).
shops(mary).
end_mod(model(609)).

begin(model(610)).
bought(spaghetti).
shops(john).
end_mod(model(610)).

begin(model(611)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(611)).

begin(model(612)).
bought(spaghetti).
shops(mary).
end_mod(model(612)).

begin(model(613)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(613)).

begin(model(614)).
bought(fish).
shops(mary).
end_mod(model(614)).

begin(model(615)).
bought(fish).
shops(mary).
end_mod(model(615)).

begin(model(616)).
bought(fish).
shops(mary).
end_mod(model(616)).

begin(model(617)).
bought(spaghetti).
shops(mary).
end_mod(model(617)).

begin(model(618)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(618)).

begin(model(619)).
end_mod(model(619)).

begin(model(620)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(620)).

begin(model(621)).
bought(fish).
shops(mary).
end_mod(model(621)).

begin(model(622)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(622)).

begin(model(623)).
bought(spaghetti).
shops(mary).
end_mod(model(623)).

begin(model(624)).
bought(fish).
shops(mary).
end_mod(model(624)).

begin(model(625)).
bought(steak).
shops(john).
end_mod(model(625)).

begin(model(626)).
bought(fish).
shops(mary).
end_mod(model(626)).

begin(model(627)).
bought(fish).
shops(mary).
end_mod(model(627)).

begin(model(628)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(628)).

begin(model(629)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(629)).

begin(model(630)).
bought(spaghetti).
shops(mary).
end_mod(model(630)).

begin(model(631)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(631)).

begin(model(632)).
bought(spaghetti).
shops(mary).
end_mod(model(632)).

begin(model(633)).
bought(spaghetti).
shops(mary).
end_mod(model(633)).

begin(model(634)).
bought(fish).
shops(mary).
end_mod(model(634)).

begin(model(635)).
bought(fish).
shops(mary).
end_mod(model(635)).

begin(model(636)).
end_mod(model(636)).

begin(model(637)).
bought(fish).
shops(mary).
end_mod(model(637)).

begin(model(638)).
bought(fish).
shops(mary).
end_mod(model(638)).

begin(model(639)).
bought(spaghetti).
shops(mary).
end_mod(model(639)).

begin(model(640)).
end_mod(model(640)).

begin(model(641)).
bought(fish).
shops(mary).
end_mod(model(641)).

begin(model(642)).
bought(fish).
shops(mary).
end_mod(model(642)).

begin(model(643)).
bought(fish).
shops(mary).
end_mod(model(643)).

begin(model(644)).
bought(spaghetti).
shops(mary).
end_mod(model(644)).

begin(model(645)).
bought(spaghetti).
shops(mary).
end_mod(model(645)).

begin(model(646)).
bought(spaghetti).
shops(mary).
end_mod(model(646)).

begin(model(647)).
bought(fish).
shops(mary).
end_mod(model(647)).

begin(model(648)).
bought(fish).
shops(mary).
end_mod(model(648)).

begin(model(649)).
bought(fish).
shops(mary).
end_mod(model(649)).

begin(model(650)).
bought(fish).
shops(mary).
end_mod(model(650)).

begin(model(651)).
bought(fish).
shops(mary).
end_mod(model(651)).

begin(model(652)).
bought(fish).
shops(mary).
end_mod(model(652)).

begin(model(653)).
bought(fish).
shops(mary).
end_mod(model(653)).

begin(model(654)).
bought(fish).
shops(mary).
end_mod(model(654)).

begin(model(655)).
bought(spaghetti).
shops(john).
end_mod(model(655)).

begin(model(656)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(656)).

begin(model(657)).
bought(fish).
shops(mary).
end_mod(model(657)).

begin(model(658)).
bought(fish).
shops(mary).
end_mod(model(658)).

begin(model(659)).
bought(spaghetti).
shops(mary).
end_mod(model(659)).

begin(model(660)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(660)).

begin(model(661)).
bought(fish).
shops(mary).
end_mod(model(661)).

begin(model(662)).
bought(fish).
shops(mary).
end_mod(model(662)).

begin(model(663)).
bought(spaghetti).
shops(mary).
end_mod(model(663)).

begin(model(664)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(664)).

begin(model(665)).
bought(fish).
shops(mary).
end_mod(model(665)).

begin(model(666)).
bought(spaghetti).
shops(mary).
end_mod(model(666)).

begin(model(667)).
bought(spaghetti).
shops(mary).
end_mod(model(667)).

begin(model(668)).
bought(fish).
shops(mary).
end_mod(model(668)).

begin(model(669)).
bought(spaghetti).
shops(mary).
end_mod(model(669)).

begin(model(670)).
bought(fish).
shops(mary).
end_mod(model(670)).

begin(model(671)).
bought(fish).
shops(mary).
end_mod(model(671)).

begin(model(672)).
bought(fish).
shops(mary).
end_mod(model(672)).

begin(model(673)).
bought(fish).
shops(mary).
end_mod(model(673)).

begin(model(674)).
bought(fish).
shops(mary).
end_mod(model(674)).

begin(model(675)).
bought(spaghetti).
shops(mary).
end_mod(model(675)).

begin(model(676)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(676)).

begin(model(677)).
bought(spaghetti).
shops(mary).
end_mod(model(677)).

begin(model(678)).
bought(fish).
shops(mary).
end_mod(model(678)).

begin(model(679)).
bought(fish).
shops(mary).
end_mod(model(679)).

begin(model(680)).
bought(spaghetti).
shops(mary).
end_mod(model(680)).

begin(model(681)).
bought(fish).
shops(mary).
end_mod(model(681)).

begin(model(682)).
bought(fish).
shops(mary).
end_mod(model(682)).

begin(model(683)).
bought(fish).
shops(mary).
end_mod(model(683)).

begin(model(684)).
bought(fish).
shops(mary).
end_mod(model(684)).

begin(model(685)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(685)).

begin(model(686)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(686)).

begin(model(687)).
bought(fish).
shops(mary).
end_mod(model(687)).

begin(model(688)).
bought(fish).
shops(mary).
end_mod(model(688)).

begin(model(689)).
bought(fish).
shops(mary).
end_mod(model(689)).

begin(model(690)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(690)).

begin(model(691)).
bought(fish).
shops(mary).
end_mod(model(691)).

begin(model(692)).
bought(fish).
shops(mary).
end_mod(model(692)).

begin(model(693)).
bought(spaghetti).
shops(mary).
end_mod(model(693)).

begin(model(694)).
bought(spaghetti).
shops(mary).
end_mod(model(694)).

begin(model(695)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(695)).

begin(model(696)).
bought(fish).
shops(mary).
end_mod(model(696)).

begin(model(697)).
bought(spaghetti).
shops(mary).
end_mod(model(697)).

begin(model(698)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(698)).

begin(model(699)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(699)).

begin(model(700)).
bought(fish).
shops(mary).
end_mod(model(700)).

begin(model(701)).
bought(fish).
shops(mary).
end_mod(model(701)).

begin(model(702)).
bought(fish).
shops(mary).
end_mod(model(702)).

begin(model(703)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(703)).

begin(model(704)).
bought(fish).
shops(mary).
end_mod(model(704)).

begin(model(705)).
bought(spaghetti).
shops(mary).
end_mod(model(705)).

begin(model(706)).
bought(spaghetti).
shops(mary).
end_mod(model(706)).

begin(model(707)).
bought(fish).
shops(mary).
end_mod(model(707)).

begin(model(708)).
bought(spaghetti).
shops(mary).
end_mod(model(708)).

begin(model(709)).
bought(fish).
shops(mary).
end_mod(model(709)).

begin(model(710)).
bought(fish).
shops(mary).
end_mod(model(710)).

begin(model(711)).
end_mod(model(711)).

begin(model(712)).
bought(fish).
shops(mary).
end_mod(model(712)).

begin(model(713)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(713)).

begin(model(714)).
bought(fish).
shops(mary).
end_mod(model(714)).

begin(model(715)).
bought(spaghetti).
shops(mary).
end_mod(model(715)).

begin(model(716)).
bought(fish).
shops(mary).
end_mod(model(716)).

begin(model(717)).
bought(spaghetti).
shops(john).
end_mod(model(717)).

begin(model(718)).
bought(fish).
shops(mary).
end_mod(model(718)).

begin(model(719)).
bought(fish).
shops(mary).
end_mod(model(719)).

begin(model(720)).
bought(fish).
shops(mary).
end_mod(model(720)).

begin(model(721)).
bought(fish).
shops(mary).
end_mod(model(721)).

begin(model(722)).
bought(spaghetti).
shops(mary).
end_mod(model(722)).

begin(model(723)).
bought(fish).
shops(mary).
end_mod(model(723)).

begin(model(724)).
bought(spaghetti).
shops(mary).
end_mod(model(724)).

begin(model(725)).
bought(fish).
shops(mary).
end_mod(model(725)).

begin(model(726)).
bought(fish).
shops(mary).
end_mod(model(726)).

begin(model(727)).
bought(spaghetti).
shops(mary).
end_mod(model(727)).

begin(model(728)).
bought(fish).
shops(mary).
end_mod(model(728)).

begin(model(729)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(729)).

begin(model(730)).
bought(spaghetti).
shops(mary).
end_mod(model(730)).

begin(model(731)).
bought(spaghetti).
shops(mary).
end_mod(model(731)).

begin(model(732)).
bought(fish).
shops(mary).
end_mod(model(732)).

begin(model(733)).
bought(fish).
shops(mary).
end_mod(model(733)).

begin(model(734)).
bought(fish).
shops(mary).
end_mod(model(734)).

begin(model(735)).
bought(spaghetti).
shops(mary).
end_mod(model(735)).

begin(model(736)).
bought(fish).
shops(mary).
end_mod(model(736)).

begin(model(737)).
bought(fish).
shops(mary).
end_mod(model(737)).

begin(model(738)).
bought(spaghetti).
shops(mary).
end_mod(model(738)).

begin(model(739)).
bought(spaghetti).
shops(mary).
end_mod(model(739)).

begin(model(740)).
bought(fish).
shops(mary).
end_mod(model(740)).

begin(model(741)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(741)).

begin(model(742)).
bought(fish).
shops(mary).
end_mod(model(742)).

begin(model(743)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(743)).

begin(model(744)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(744)).

begin(model(745)).
bought(spaghetti).
shops(mary).
end_mod(model(745)).

begin(model(746)).
bought(spaghetti).
shops(mary).
end_mod(model(746)).

begin(model(747)).
bought(fish).
shops(mary).
end_mod(model(747)).

begin(model(748)).
bought(spaghetti).
shops(mary).
end_mod(model(748)).

begin(model(749)).
bought(fish).
shops(mary).
end_mod(model(749)).

begin(model(750)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(750)).

begin(model(751)).
bought(spaghetti).
shops(mary).
end_mod(model(751)).

begin(model(752)).
bought(spaghetti).
shops(mary).
end_mod(model(752)).

begin(model(753)).
bought(fish).
shops(mary).
end_mod(model(753)).

begin(model(754)).
bought(fish).
shops(mary).
end_mod(model(754)).

begin(model(755)).
bought(fish).
shops(mary).
end_mod(model(755)).

begin(model(756)).
bought(fish).
shops(mary).
end_mod(model(756)).

begin(model(757)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(757)).

begin(model(758)).
bought(fish).
shops(mary).
end_mod(model(758)).

begin(model(759)).
bought(fish).
shops(mary).
end_mod(model(759)).

begin(model(760)).
bought(fish).
shops(mary).
end_mod(model(760)).

begin(model(761)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(761)).

begin(model(762)).
bought(spaghetti).
shops(mary).
end_mod(model(762)).

begin(model(763)).
bought(fish).
shops(mary).
end_mod(model(763)).

begin(model(764)).
bought(fish).
shops(mary).
end_mod(model(764)).

begin(model(765)).
bought(fish).
shops(mary).
end_mod(model(765)).

begin(model(766)).
bought(fish).
shops(mary).
end_mod(model(766)).

begin(model(767)).
bought(fish).
shops(mary).
end_mod(model(767)).

begin(model(768)).
bought(spaghetti).
shops(mary).
end_mod(model(768)).

begin(model(769)).
bought(fish).
shops(mary).
end_mod(model(769)).

begin(model(770)).
bought(spaghetti).
shops(mary).
end_mod(model(770)).

begin(model(771)).
bought(fish).
shops(mary).
end_mod(model(771)).

begin(model(772)).
bought(fish).
shops(mary).
end_mod(model(772)).

begin(model(773)).
bought(spaghetti).
shops(mary).
end_mod(model(773)).

begin(model(774)).
bought(fish).
shops(mary).
end_mod(model(774)).

begin(model(775)).
bought(fish).
shops(mary).
end_mod(model(775)).

begin(model(776)).
bought(spaghetti).
shops(mary).
end_mod(model(776)).

begin(model(777)).
bought(spaghetti).
shops(mary).
end_mod(model(777)).

begin(model(778)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(778)).

begin(model(779)).
bought(spaghetti).
shops(john).
end_mod(model(779)).

begin(model(780)).
bought(fish).
shops(mary).
end_mod(model(780)).

begin(model(781)).
bought(spaghetti).
shops(mary).
end_mod(model(781)).

begin(model(782)).
bought(fish).
shops(mary).
end_mod(model(782)).

begin(model(783)).
bought(fish).
shops(mary).
end_mod(model(783)).

begin(model(784)).
bought(fish).
shops(mary).
end_mod(model(784)).

begin(model(785)).
bought(spaghetti).
shops(mary).
end_mod(model(785)).

begin(model(786)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(786)).

begin(model(787)).
bought(fish).
shops(mary).
end_mod(model(787)).

begin(model(788)).
bought(spaghetti).
shops(mary).
end_mod(model(788)).

begin(model(789)).
end_mod(model(789)).

begin(model(790)).
bought(fish).
shops(mary).
end_mod(model(790)).

begin(model(791)).
bought(fish).
shops(mary).
end_mod(model(791)).

begin(model(792)).
bought(fish).
shops(mary).
end_mod(model(792)).

begin(model(793)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(793)).

begin(model(794)).
bought(spaghetti).
shops(mary).
end_mod(model(794)).

begin(model(795)).
bought(fish).
shops(mary).
end_mod(model(795)).

begin(model(796)).
bought(fish).
shops(mary).
end_mod(model(796)).

begin(model(797)).
bought(fish).
shops(mary).
end_mod(model(797)).

begin(model(798)).
bought(spaghetti).
shops(mary).
end_mod(model(798)).

begin(model(799)).
bought(fish).
shops(mary).
end_mod(model(799)).

begin(model(800)).
bought(fish).
shops(mary).
end_mod(model(800)).

begin(model(801)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(801)).

begin(model(802)).
bought(spaghetti).
shops(mary).
end_mod(model(802)).

begin(model(803)).
bought(fish).
shops(mary).
end_mod(model(803)).

begin(model(804)).
bought(spaghetti).
shops(mary).
end_mod(model(804)).

begin(model(805)).
bought(spaghetti).
shops(mary).
end_mod(model(805)).

begin(model(806)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(806)).

begin(model(807)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(807)).

begin(model(808)).
bought(fish).
shops(mary).
end_mod(model(808)).

begin(model(809)).
bought(fish).
shops(mary).
end_mod(model(809)).

begin(model(810)).
bought(fish).
shops(mary).
end_mod(model(810)).

begin(model(811)).
bought(fish).
shops(mary).
end_mod(model(811)).

begin(model(812)).
bought(fish).
shops(mary).
end_mod(model(812)).

begin(model(813)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(813)).

begin(model(814)).
bought(fish).
shops(mary).
end_mod(model(814)).

begin(model(815)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(815)).

begin(model(816)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(816)).

begin(model(817)).
bought(fish).
shops(mary).
end_mod(model(817)).

begin(model(818)).
bought(spaghetti).
shops(mary).
end_mod(model(818)).

begin(model(819)).
bought(spaghetti).
shops(mary).
end_mod(model(819)).

begin(model(820)).
bought(fish).
shops(mary).
end_mod(model(820)).

begin(model(821)).
bought(spaghetti).
shops(john).
end_mod(model(821)).

begin(model(822)).
bought(fish).
shops(mary).
end_mod(model(822)).

begin(model(823)).
bought(fish).
shops(mary).
end_mod(model(823)).

begin(model(824)).
bought(fish).
shops(mary).
end_mod(model(824)).

begin(model(825)).
bought(fish).
shops(mary).
end_mod(model(825)).

begin(model(826)).
bought(fish).
shops(mary).
end_mod(model(826)).

begin(model(827)).
bought(fish).
shops(mary).
end_mod(model(827)).

begin(model(828)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(828)).

begin(model(829)).
bought(fish).
shops(mary).
end_mod(model(829)).

begin(model(830)).
bought(fish).
shops(mary).
end_mod(model(830)).

begin(model(831)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(831)).

begin(model(832)).
bought(spaghetti).
shops(mary).
end_mod(model(832)).

begin(model(833)).
bought(fish).
shops(mary).
end_mod(model(833)).

begin(model(834)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(834)).

begin(model(835)).
bought(fish).
shops(mary).
end_mod(model(835)).

begin(model(836)).
bought(fish).
shops(mary).
end_mod(model(836)).

begin(model(837)).
bought(spaghetti).
shops(mary).
end_mod(model(837)).

begin(model(838)).
bought(fish).
shops(mary).
end_mod(model(838)).

begin(model(839)).
bought(fish).
shops(mary).
end_mod(model(839)).

begin(model(840)).
bought(fish).
shops(mary).
end_mod(model(840)).

begin(model(841)).
bought(fish).
shops(mary).
end_mod(model(841)).

begin(model(842)).
bought(fish).
shops(mary).
end_mod(model(842)).

begin(model(843)).
bought(fish).
shops(mary).
end_mod(model(843)).

begin(model(844)).
bought(fish).
shops(mary).
end_mod(model(844)).

begin(model(845)).
bought(fish).
shops(mary).
end_mod(model(845)).

begin(model(846)).
bought(fish).
shops(mary).
end_mod(model(846)).

begin(model(847)).
bought(spaghetti).
shops(mary).
end_mod(model(847)).

begin(model(848)).
bought(spaghetti).
shops(mary).
end_mod(model(848)).

begin(model(849)).
bought(fish).
shops(mary).
end_mod(model(849)).

begin(model(850)).
bought(spaghetti).
shops(mary).
end_mod(model(850)).

begin(model(851)).
bought(fish).
shops(mary).
end_mod(model(851)).

begin(model(852)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(852)).

begin(model(853)).
bought(fish).
shops(mary).
end_mod(model(853)).

begin(model(854)).
bought(spaghetti).
shops(mary).
end_mod(model(854)).

begin(model(855)).
bought(fish).
shops(mary).
end_mod(model(855)).

begin(model(856)).
bought(fish).
shops(mary).
end_mod(model(856)).

begin(model(857)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(857)).

begin(model(858)).
bought(fish).
shops(mary).
end_mod(model(858)).

begin(model(859)).
bought(spaghetti).
shops(mary).
end_mod(model(859)).

begin(model(860)).
bought(spaghetti).
shops(mary).
end_mod(model(860)).

begin(model(861)).
bought(fish).
shops(mary).
end_mod(model(861)).

begin(model(862)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(862)).

begin(model(863)).
bought(fish).
shops(mary).
end_mod(model(863)).

begin(model(864)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(864)).

begin(model(865)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(865)).

begin(model(866)).
bought(spaghetti).
shops(mary).
end_mod(model(866)).

begin(model(867)).
bought(fish).
shops(mary).
end_mod(model(867)).

begin(model(868)).
bought(fish).
shops(mary).
end_mod(model(868)).

begin(model(869)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(869)).

begin(model(870)).
bought(fish).
shops(mary).
end_mod(model(870)).

begin(model(871)).
bought(spaghetti).
shops(mary).
end_mod(model(871)).

begin(model(872)).
bought(fish).
shops(mary).
end_mod(model(872)).

begin(model(873)).
bought(spaghetti).
shops(mary).
end_mod(model(873)).

begin(model(874)).
bought(spaghetti).
shops(mary).
end_mod(model(874)).

begin(model(875)).
bought(spaghetti).
shops(mary).
end_mod(model(875)).

begin(model(876)).
bought(fish).
shops(mary).
end_mod(model(876)).

begin(model(877)).
bought(steak).
shops(john).
end_mod(model(877)).

begin(model(878)).
bought(steak).
shops(john).
end_mod(model(878)).

begin(model(879)).
bought(fish).
shops(mary).
end_mod(model(879)).

begin(model(880)).
bought(spaghetti).
shops(mary).
end_mod(model(880)).

begin(model(881)).
bought(spaghetti).
shops(john).
end_mod(model(881)).

begin(model(882)).
bought(fish).
shops(mary).
end_mod(model(882)).

begin(model(883)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(883)).

begin(model(884)).
bought(fish).
shops(mary).
end_mod(model(884)).

begin(model(885)).
bought(fish).
shops(mary).
end_mod(model(885)).

begin(model(886)).
bought(fish).
shops(mary).
end_mod(model(886)).

begin(model(887)).
bought(fish).
shops(mary).
end_mod(model(887)).

begin(model(888)).
bought(spaghetti).
shops(mary).
end_mod(model(888)).

begin(model(889)).
bought(fish).
shops(mary).
end_mod(model(889)).

begin(model(890)).
bought(fish).
shops(mary).
end_mod(model(890)).

begin(model(891)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(891)).

begin(model(892)).
bought(spaghetti).
shops(mary).
end_mod(model(892)).

begin(model(893)).
bought(fish).
shops(mary).
end_mod(model(893)).

begin(model(894)).
bought(fish).
shops(mary).
end_mod(model(894)).

begin(model(895)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(895)).

begin(model(896)).
bought(spaghetti).
shops(mary).
end_mod(model(896)).

begin(model(897)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(897)).

begin(model(898)).
bought(fish).
shops(mary).
end_mod(model(898)).

begin(model(899)).
bought(fish).
shops(mary).
end_mod(model(899)).

begin(model(900)).
bought(fish).
shops(mary).
end_mod(model(900)).

begin(model(901)).
bought(fish).
shops(mary).
end_mod(model(901)).

begin(model(902)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(902)).

begin(model(903)).
bought(steak).
shops(john).
end_mod(model(903)).

begin(model(904)).
bought(fish).
shops(mary).
end_mod(model(904)).

begin(model(905)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(905)).

begin(model(906)).
bought(spaghetti).
shops(mary).
end_mod(model(906)).

begin(model(907)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(907)).

begin(model(908)).
bought(fish).
shops(mary).
end_mod(model(908)).

begin(model(909)).
bought(fish).
shops(mary).
end_mod(model(909)).

begin(model(910)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(910)).

begin(model(911)).
bought(fish).
shops(mary).
end_mod(model(911)).

begin(model(912)).
bought(fish).
shops(mary).
end_mod(model(912)).

begin(model(913)).
bought(fish).
shops(mary).
end_mod(model(913)).

begin(model(914)).
bought(fish).
shops(mary).
end_mod(model(914)).

begin(model(915)).
bought(fish).
shops(mary).
end_mod(model(915)).

begin(model(916)).
bought(fish).
shops(mary).
end_mod(model(916)).

begin(model(917)).
bought(spaghetti).
shops(mary).
end_mod(model(917)).

begin(model(918)).
end_mod(model(918)).

begin(model(919)).
bought(fish).
shops(mary).
end_mod(model(919)).

begin(model(920)).
bought(fish).
shops(mary).
end_mod(model(920)).

begin(model(921)).
bought(fish).
shops(mary).
end_mod(model(921)).

begin(model(922)).
bought(fish).
shops(mary).
end_mod(model(922)).

begin(model(923)).
bought(steak).
shops(john).
end_mod(model(923)).

begin(model(924)).
bought(fish).
shops(mary).
end_mod(model(924)).

begin(model(925)).
bought(spaghetti).
shops(mary).
end_mod(model(925)).

begin(model(926)).
bought(fish).
shops(mary).
end_mod(model(926)).

begin(model(927)).
bought(fish).
shops(mary).
end_mod(model(927)).

begin(model(928)).
bought(spaghetti).
shops(mary).
end_mod(model(928)).

begin(model(929)).
bought(fish).
shops(mary).
end_mod(model(929)).

begin(model(930)).
end_mod(model(930)).

begin(model(931)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(931)).

begin(model(932)).
bought(fish).
shops(mary).
end_mod(model(932)).

begin(model(933)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(933)).

begin(model(934)).
bought(fish).
shops(mary).
end_mod(model(934)).

begin(model(935)).
bought(fish).
shops(mary).
end_mod(model(935)).

begin(model(936)).
bought(fish).
shops(mary).
end_mod(model(936)).

begin(model(937)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(937)).

begin(model(938)).
end_mod(model(938)).

begin(model(939)).
bought(spaghetti).
shops(mary).
end_mod(model(939)).

begin(model(940)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(940)).

begin(model(941)).
bought(fish).
shops(mary).
end_mod(model(941)).

begin(model(942)).
bought(fish).
shops(mary).
end_mod(model(942)).

begin(model(943)).
bought(spaghetti).
shops(mary).
end_mod(model(943)).

begin(model(944)).
bought(spaghetti).
shops(mary).
end_mod(model(944)).

begin(model(945)).
bought(spaghetti).
shops(mary).
end_mod(model(945)).

begin(model(946)).
bought(spaghetti).
shops(mary).
end_mod(model(946)).

begin(model(947)).
bought(fish).
shops(mary).
end_mod(model(947)).

begin(model(948)).
bought(spaghetti).
shops(mary).
end_mod(model(948)).

begin(model(949)).
bought(fish).
shops(mary).
end_mod(model(949)).

begin(model(950)).
bought(spaghetti).
shops(mary).
end_mod(model(950)).

begin(model(951)).
bought(spaghetti).
shops(mary).
end_mod(model(951)).

begin(model(952)).
bought(fish).
shops(mary).
end_mod(model(952)).

begin(model(953)).
bought(fish).
shops(mary).
end_mod(model(953)).

begin(model(954)).
bought(fish).
shops(mary).
end_mod(model(954)).

begin(model(955)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(955)).

begin(model(956)).
bought(fish).
shops(mary).
end_mod(model(956)).

begin(model(957)).
bought(fish).
shops(mary).
end_mod(model(957)).

begin(model(958)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(958)).

begin(model(959)).
bought(fish).
shops(mary).
end_mod(model(959)).

begin(model(960)).
bought(fish).
shops(mary).
end_mod(model(960)).

begin(model(961)).
bought(fish).
shops(mary).
end_mod(model(961)).

begin(model(962)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(962)).

begin(model(963)).
bought(fish).
shops(mary).
end_mod(model(963)).

begin(model(964)).
bought(fish).
shops(mary).
end_mod(model(964)).

begin(model(965)).
bought(fish).
shops(mary).
end_mod(model(965)).

begin(model(966)).
bought(fish).
shops(mary).
end_mod(model(966)).

begin(model(967)).
bought(spaghetti).
shops(mary).
end_mod(model(967)).

begin(model(968)).
bought(fish).
shops(mary).
end_mod(model(968)).

begin(model(969)).
bought(fish).
shops(mary).
end_mod(model(969)).

begin(model(970)).
bought(spaghetti).
shops(mary).
end_mod(model(970)).

begin(model(971)).
bought(spaghetti).
shops(mary).
end_mod(model(971)).

begin(model(972)).
bought(spaghetti).
shops(mary).
end_mod(model(972)).

begin(model(973)).
bought(spaghetti).
shops(mary).
end_mod(model(973)).

begin(model(974)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(974)).

begin(model(975)).
bought(fish).
shops(mary).
end_mod(model(975)).

begin(model(976)).
bought(fish).
shops(mary).
end_mod(model(976)).

begin(model(977)).
bought(fish).
shops(mary).
end_mod(model(977)).

begin(model(978)).
end_mod(model(978)).

begin(model(979)).
bought(fish).
shops(mary).
end_mod(model(979)).

begin(model(980)).
bought(fish).
shops(mary).
end_mod(model(980)).

begin(model(981)).
bought(spaghetti).
shops(mary).
end_mod(model(981)).

begin(model(982)).
bought(fish).
shops(mary).
end_mod(model(982)).

begin(model(983)).
bought(spaghetti).
bought(steak).
shops(mary).
shops(john).
end_mod(model(983)).

begin(model(984)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(984)).

begin(model(985)).
bought(spaghetti).
shops(mary).
end_mod(model(985)).

begin(model(986)).
bought(fish).
shops(mary).
end_mod(model(986)).

begin(model(987)).
bought(fish).
shops(mary).
end_mod(model(987)).

begin(model(988)).
bought(fish).
bought(spaghetti).
shops(mary).
shops(john).
end_mod(model(988)).

begin(model(989)).
bought(spaghetti).
shops(john).
end_mod(model(989)).

begin(model(990)).
bought(fish).
shops(mary).
end_mod(model(990)).

begin(model(991)).
bought(fish).
shops(mary).
end_mod(model(991)).

begin(model(992)).
bought(spaghetti).
shops(mary).
end_mod(model(992)).

begin(model(993)).
bought(fish).
shops(mary).
end_mod(model(993)).

begin(model(994)).
bought(spaghetti).
shops(mary).
end_mod(model(994)).

begin(model(995)).
bought(fish).
shops(mary).
end_mod(model(995)).

begin(model(996)).
bought(fish).
shops(mary).
end_mod(model(996)).

begin(model(997)).
bought(fish).
shops(mary).
end_mod(model(997)).

begin(model(998)).
bought(fish).
shops(mary).
end_mod(model(998)).

begin(model(999)).
bought(fish).
shops(mary).
end_mod(model(999)).

begin(model(1000)).
bought(fish).
bought(steak).
shops(mary).
shops(john).
end_mod(model(1000)).
