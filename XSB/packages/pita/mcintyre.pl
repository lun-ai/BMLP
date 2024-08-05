:- op(600,xfy,'::').
:-  op(600,xfy,'~').
:-  op(500,xfx,'~=').
:-  op(1200,xfy,':=').
:-  op(1150,fx,mcaction).

/** <module> mcintyre

This module performs reasoning over Logic Programs with Annotated
Disjunctions and CP-Logic programs.
It reads probabilistic program and computes the probability of queries
using sampling.

See https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html for
details.

@author Fabrizio Riguzzi
@license Artistic License 2.0 https://opensource.org/licenses/Artistic-2.0
@copyright Fabrizio Riguzzi
*/
  :- compiler_options([spec_off,verbo]).

%:- consult(bddem, [cc_opts('-Icudd-3.0.0 -Icudd-3.0.0/cudd -Icudd-3.0.0/util -fPIC -DBP_FREE -O3 -fomit-frame-pointer -Wall -g -O2')]).


:- import append/3, member/2, length/2, ith/3, delete_ith/4, reverse/2,
  between/3 from basics.
:- import format/3 from format.
:- import concat_atom/2 from string.
:- import
	assoc_to_list/2,		% Assoc -> List
	assoc_vals_to_list/2,		% Assoc -> List
	empty_assoc/1,			% -> Assoc
	del_assoc/4,			% Key x Assoc x Val -> Assoc
	del_max_assoc/4,		% Assoc -> Key x Val x Assoc
	del_min_assoc/4,		% Assoc -> Key x Val x Assoc
	gen_assoc/3,			% Key x Assoc x Val
	get_assoc/3,			% Key x Assoc -> Val
	get_assoc/5,			% Key x Assoc x Val -> Assoc x Val
	get_next_assoc/4,		% Key x Assoc -> Key x Val
	get_prev_assoc/4,		% Key x Assoc -> Key x Val
	is_assoc/1,			% Assoc ->
	list_to_assoc/2,		% List -> Assoc
%%	map_assoc/2,			% Goal x Assoc ->
%%	map_assoc/3,			% Goal x Assoc -> Assoc
	max_assoc/3,			% Assoc -> Key x Val
	min_assoc/3,			% Assoc -> Key x Val
	ord_list_to_assoc/2,		% List -> Assoc
	ord_list_to_assoc/4,
	put_assoc/4			% Key x Assoc x Val -> Assoc
  from assoc_xsb.
:- import maplist/2, maplist/3, maplist/4, foldl/4, partition/4,
          nth0/3, nth0/4, nth1/3, nth1/4, option/3,
            numbervars/1,
            random/3, numlist/3, sum_list/2
       from swi.

:- import random/1 from random.
:- import numbervars/3 from num_vars.

% :-predicate_options(mc_prob/3,3,[bar(-)]).
% :-predicate_options(mc_sample/4,4,[successes(-),failures(-),bar(-)]).
% :-predicate_options(mc_mh_sample/5,5,[successes(-),failures(-),mix(+),lag(+)]).
% :-predicate_options(mc_gibbs_sample/5,5,[successes(-),failures(-),mix(+),block(+)]).
% :-predicate_options(mc_gibbs_sample/4,4,[successes(-),failures(-),mix(+),block(+)]).
% :-predicate_options(mc_rejection_sample/5,5,[successes(-),failures(-)]).
% :-predicate_options(mc_sample_arg/5,5,[bar(-)]).
% :-predicate_options(mc_rejection_sample_arg/6,6,[bar(-)]).
% :-predicate_options(mc_mh_sample_arg/6,6,[mix(+),lag(+),bar(-)]).
% :-predicate_options(mc_gibbs_sample_arg/6,6,[mix(+),bar(-),block(+)]).
% :-predicate_options(mc_gibbs_sample_arg/5,5,[mix(+),bar(-),block(+)]).
% :-predicate_options(mc_sample_arg_first/5,5,[bar(-)]).
% :-predicate_options(mc_sample_arg_one/5,5,[bar(-)]).
% :-predicate_options(mc_mh_expectation/6,6,[mix(+),lag(+)]).
% :-predicate_options(mc_gibbs_expectation/6,6,[mix(+),block(+)]).
% :-predicate_options(mc_gibbs_expectation/5,5,[mix(+),block(+)]).
% :-predicate_options(histogram/3,3,[max(+),min(+),nbins(+)]).
% :-predicate_options(density/3,3,[max(+),min(+),nbins(+)]).
% :-predicate_options(density2d/3,3,[xmax(+),xmin(+),ymax(+),ymin(+),nbins(+)]).



:- dynamic sampled/3, sampled_g/2, sampled_g/1.

/* k
 * -
 * This parameter shows the amount of items of the same type to consider at once.
 *
 * Default value:	500
 * Applies to:		bestfirst, bestk, montecarlo
 */
default_setting_mc(k, 500).
/* min_error
 * ---------
 * This parameter shows the threshold for the probability interval.
 *
 * Default value:	0.02
 * Applies to:		bestfirst, montecarlo
 */
default_setting_mc(min_error, 0.02).

default_setting_mc(max_samples,5.0e4).


default_setting_mc(epsilon_parsing, 1.0e-5).
/* on, off */

default_setting_mc(compiling,off).

:-set_prolog_flag(unknown,warning).

default_setting_mc(depth_bound,false).  %if true, it limits the derivation of the example to the value of 'depth'
default_setting_mc(depth,2).
default_setting_mc(single_var,false). %false:1 variable for every grounding of a rule; true: 1 variable for rule (even if a rule has more groundings),simpler.

/**
 * mc_load(++File:atom) is det
 *
 * Loads File.lpad if it exists, otherwise loads File.cpl if it exists.
 */
mc_load(File):-
  atomic_concat(File,'.lpad',FileLPAD),
  (exists_file(FileLPAD)->
    mc_load_file(FileLPAD)
  ;
    atomic_concat(File,'.cpl',FileCPL),
    (exists_file(FileCPL)->
      mc_load_file(FileCPL)
    )
  ).

/**
 * mc_load_file(++FileWithExtension:atom) is det
 *
 * Loads FileWithExtension.
 */
mc_load_file(File):-
  begin_lpad_pred,
  user:consult(File),
  end_lpad_pred.

/**
 * s(:Query:atom,-Probability:float) is nondet
 *
 * The predicate computes the probability of the ground query Query.
 * If Query is not ground, it returns in backtracking all instantiations of
 * Query together with their probabilities
 */
s(Mo:Goal,P):-
  Mo:local_mc_setting(min_error, MinError),
  Mo:local_mc_setting(k, K),
% Resetting the clocks...
% Performing resolution...
  copy_term(Goal,Goal1),
  numbervars(Goal1),
  save_samples(Mo,Goal1),
  montecarlo_cycle(0, 0, Mo:Goal, K, MinError, _Samples, _Lower, P, _Upper),
  !,
  erase_samples,
  restore_samples_delete_copy(Mo,Goal1).

save_samples_tab(M,I,S):-
  sampled(R,Sub,V),
  assert(M:mem(I,S,R,Sub,V)),
  retract(sampled(R,Sub,V)),
  fail.

save_samples_tab(M,I,S):-
  sampled_g(Sub,V),
  assert(M:mem(I,S,rw,Sub,V)),
  retract(sampled_g(Sub,V)),
  fail.

save_samples_tab(M,I,S):-
  sampled_g(Sub),
  assert(M:mem(I,S,r,Sub,1)),
  retract(sampled_g(Sub)),
  fail.

save_samples_tab(_M,_I,_S).

save_samples(M,I,S):-
  sampled(R,Sub,V),
  assert(M:mem(I,S,R,Sub,V)),
  retract(sampled(R,Sub,V)),
  fail.

save_samples(_M,_I,_S):-
  retractall(sampled_g(_,_)),
  retractall(sampled_g(_)).

save_samples(M,G):-
  sampled(R,Sub,V),
  assert(M:mem(G,R,Sub,V)),
  erase(sampled(R,Sub,V)),
  fail.

save_samples(_M,_G).

restore_samples(M,I,S):-
  M:mem(I,S,R,Sub,V),
  assert_samp(R,Sub,V),
  fail.

restore_samples(_M,_I,_S).

assert_samp(r,Sub,_V):-!,
  assertz(sampled_g(Sub)).

assert_samp(rw,Sub,V):-!,
  assertz(sampled_g(Sub,V)).

assert_samp(R,Sub,V):-
  assertz(sampled(R,Sub,V)).


restore_samples(M,G):-
  M:mem(G,R,Sub,V),
  assertz(sampled(R,Sub,V)),
  fail.

restore_samples(_M,_G).


restore_samples_delete_copy(M,G):-
  retract(M:mem(G,R,Sub,V)),
  assertz(sampled(R,Sub,V)),
  fail.

restore_samples_delete_copy(_M,_G).

save_samples_copy(M,G):-
  sampled(R,Sub,V),
  assert(M:mem(G,R,Sub,V)),
  fail.

save_samples_copy(_M,_G).

delete_samples_copy(M,G):-
  retract(M:mem(G,_R,_Sub,_V)),
  fail.

delete_samples_copy(_M,_G).

count_samples(N):-
  findall(a,sampled(_Key,_Sub,_Val),L),
  length(L,N).


resample(0):-!.

resample(N):-
  findall(sampled(Key,Sub,Val),sampled(Key,Sub,Val),L),
  sample_one(L,S),
  retractall(S),
  N1 is N-1,
  resample(N1).


erase_samples:-
  retractall(sampled(_,_,_)),
  retractall(sampled_g(_,_)),
  retractall(sampled_g(_)).
print_samples:-
  sampled(Key,Sub,Val),
  write(Key-(Sub,Val)),nl,
  fail.

print_samples:-
  write(end),nl.

montecarlo_cycle(N0, S0, M:Goals, K, MinError, Samples, Lower, Prob, Upper):-!,
  montecarlo(K,N0, S0, M:Goals, N, S),
  P is S / N,
  D is N - S,
  Semi is 1.95996 * sqrt(P * (1 - P) / N),
  Int is 2 * Semi,
  M:local_mc_setting(max_samples,MaxSamples),
  /*   N * P > 5;   N * S / N > 5;   S > 5
  *   N (1 - P) > 5;   N (1 - S / N) > 5;   N (N - S) / N > 5;   N - S > 5
  */
  %format("Batch: samples ~d positive ~d interval ~f~n",[N,S,Int]),
% flush_output,
  (((S > 5, D > 5, (Int < MinError; Int =:= 0));
    ((Int < MinError; Int =:= 0),N>MaxSamples)) ->
    Samples is N,
    Lower is P - Semi,
    Prob is P,
    Upper is P + Semi %,
%   writeln(Semi)
   ;
     montecarlo_cycle(N, S, M:Goals, K, MinError, Samples, Lower, Prob, Upper)
   ).

montecarlo(0,N,S , _Goals,N,S):-!.

montecarlo(K1,Count, Success, M:Goals,N1,S1):-
  erase_samples,
  copy_term(Goals,Goals1),
  (M:Goals1->
    Valid=1
  ;
    Valid=0
  ),
  N is Count + 1,
  S is Success + Valid,
  %format("Sample ~d Valid ~d~n",[N,Valid]),
  %flush_output,
  K2 is K1-1,
  montecarlo(K2,N, S, M:Goals, N1,S1).


rejection_montecarlo(0,N,S , _Goals,_Ev,N,S):-!.

rejection_montecarlo(K1,Count, Success, M:Goals,M:Ev,N1,S1):-
  erase_samples,
  copy_term(Ev,Ev1),
  (M:Ev1->
    copy_term(Goals,Goals1),
    (M:Goals1->
      Succ=1
    ;
      Succ=0
    ),
    N is Count + 1,
    S is Success + Succ,
  %format("Sample ~d Valid ~d~n",[N,Valid]),
  %flush_output,
    K2 is K1-1
  ;
    N = Count,
    S = Success,
    K2 = K1
  ),
  rejection_montecarlo(K2,N, S, M:Goals,M:Ev, N1,S1).

mh_montecarlo(_L,K,_NC0,N,S,Succ0,Succ0, _Goals,_Ev,N,S):-
  K=<0,!.

mh_montecarlo(L,K0,NC0,N0, S0,Succ0, SuccNew,M:Goal, M:Evidence, N, S):-
  resample(L),
  copy_term(Evidence,Ev1),
  (M:Ev1->
    copy_term(Goal,Goal1),
    (M:Goal1->
      Succ1=1
    ;
      Succ1=0
    ),
    count_samples(NC1),
    (accept(NC0,NC1)->
      Succ = Succ1,
      delete_samples_copy(M,Goal),
      save_samples_copy(M,Goal)
    ;
      Succ = Succ0,
      erase_samples,
      restore_samples(M,Goal)
    ),
    N1 is N0 + 1,
    S1 is S0 + Succ,
  %format("Sample ~d Valid ~d~n",[N,Valid]),
  %flush_output,
    K1 is K0-1
  ;
    N1 = N0,
    S1 = S0,
    K1 = K0,
    NC1 = NC0,
    Succ = Succ0,
    erase_samples,
    restore_samples(M,Goal)
  ),
  mh_montecarlo(L,K1,NC1,N1, S1,Succ, SuccNew,M:Goal,M:Evidence, N,S).

accept(NC1,NC2):-
  P is min(1,NC1/NC2),
  random(P0),
  P>P0.

/**
 * mc_prob(:Query:atom,-Probability:float,+Options:list) is det
 *
 * The predicate computes the probability of the query Query
 * If Query is not ground, it considers it as an existential query
 * and returns the probability that there is a satisfying assignment to
 * the query.
 *
 * Options is a list of options, the following are recognised by mc_prob/3:
 * * bar(-BarChart:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with a bar for the
 *   probability of success and a bar for the probability of failure.
 */
mc_prob(Goal,P,Options):-
  M=usermod,
  s(M:Goal,P),
  option(bar(Chart),Options,no),
  (nonvar(Chart)->
    true
  ;
    bar(P,Chart)
  ).
/**
 * mc_prob(:Query:atom,-Probability:float) is det
 *
 * Equivalent to mc_prob/2 with an empty option list.
 */
mc_prob(Goal,P):-
  mc_prob(Goal,P,[]).

/**
 * mc_sample(:Query:atom,+Samples:int,-Probability:float,+Options:list) is det
 *
 * The predicate samples Query a number of Samples times and returns
 * the resulting Probability (Successes/Samples)
 * If Query is not ground, it considers it as an existential query
 *
 * Options is a list of options, the following are recognised by mc_sample/4:
 * * successes(-Successes:int)
 *   Number of successes
 * * failures(-Failures:int)
 *   Number of failures
 * * bar(-BarChart:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with a bar for the
 *   number of successes and a bar for the number of failures.
 */
mc_sample(Goal,S,P,Options):-
  M=usermod,
  option(successes(T),Options,_T),
  option(failures(F),Options,_F),
  mc_sample(M:Goal,S,T,F,P),
  option(bar(Chart),Options,no),
  (nonvar(Chart)->
    true
  ;
    bar(T,F,Chart)
  ).

/**
 * mc_sample(:Query:atom,+Samples:int,-Probability:float) is det
 * 
 * Equivalent to mc_sample/4 with an empty option list.
 */
mc_sample(Goal,S,P):-
  mc_sample(Goal,S,P,[]).

/**
 * mc_sample(:Query:atom,+Samples:int,-Successes:int,-Failures:int,-Probability:float) is det
 *
 * The predicate samples Query  a number of Samples times and returns
 * the number of Successes, of Failures and the
 * Probability (Successes/Samples)
 * If Query is not ground, it considers it as an existential query
 */
mc_sample(Goal,S,T,F,P):-
  M=usermod,
  copy_term(Goal,Goal1),
  numbervars(Goal1),
  save_samples(M,Goal1),
  montecarlo(S,0, 0, M:Goal, N, T),
  P is T / N,
  F is N - T,
  erase_samples,
  restore_samples_delete_copy(M,Goal1).

/**
 * mc_rejection_sample(:Query:atom,:Evidence:atom,+Samples:int,-Probability:float,+Options:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true and returns
 * the Probability of Query.
 * It performs rejection sampling: if in a sample Evidence is false, the
 * sample is discarded.
 * If Query/Evidence are not ground, it considers them an existential queries.
 *
 * Options is a list of options, the following are recognised by mc_rejection_sample/5:
 * * successes(-Successes:int)
 *   Number of succeses
 * * failures(-Failures:int)
 *   Number of failueres
 */
mc_rejection_sample(Goal,Evidence,S,P,Options):-
  M=usermod,
  option(successes(T),Options,_T),
  option(failures(F),Options,_F),
  mc_rejection_sample(M:Goal,M:Evidence,S,T,F,P).

/**
 * mc_rejection_sample(:Query:atom,:Evidence:atom,+Samples:int,-Probability:float) is det
 *
 * Equivalent to mc_rejection_sample/5 with an empty option list.
 */
mc_rejection_sample(Goal,Evidence,S,P):-
  mc_rejection_sample(Goal,Evidence,S,P,[]).
 /**
 * mc_rejection_sample(:Query:atom,:Evidence:atom,+Samples:int,-Successes:int,-Failures:int,-Probability:float) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true and returns
 * the number of Successes, of Failures and the
 * Probability (Successes/Samples).
 * It performs rejection sampling: if in a sample Evidence is false, the
 * sample is discarded.
 * If Query/Evidence are not ground, it considers them an existential queries.
 */
mc_rejection_sample(M:Goal,M:Evidence0,S,T,F,P):-
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  rejection_montecarlo(S,0, 0, M:Goal,M:Evidence, N, T),
  P is T / N,
  F is N - T,
  erase_samples,
  maplist(retract,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).

deal_with_ev(Ev,M,EvGoal,UC,CA):-
  list2and(EvL,Ev),
  partition(ac,EvL,ActL,EvNoActL),
  deal_with_actions(ActL,M,UC,CA),
  list2and(EvNoActL,EvGoal).

deal_with_actions(ActL,M,UC,CA):-
  empty_assoc(AP0),
  foldl(get_pred_const,ActL,AP0,AP),
  assoc_to_list(AP,LP),
  maplist(update_clauses(M),LP,UCL,CAL),
  partition(nac,ActL,_NActL,PActL),
  maplist(assert_actions(M),PActL,ActRefs),
  append([ActRefs|UCL],UC),
  append(CAL,CA).

assert_actions(M,do(A),A):-
  M:assertz(A).

update_clauses(M,P/0- _,[],LCA):-!,
  findall((P:-B),M:clause(P,B),UC),
  findall((P:-B),M:clause(P,B),LCA),
  maplist(retract,UC).

update_clauses(M,P/A-Constants,UC,CA):-
  functor(G,P,A),
  G=..[_|Args],
  findall((G,B),M:clause(G,B),LC),
  maplist(get_const(Args),Constants,ConstraintsL),
  list2and(ConstraintsL,Constraints),
  maplist(add_cons(G,Constraints,M),LC,UC,CA).

add_cons(G,C,M,(H,B),(H:-(C1,B)),(H:-B)):-
  copy_term((G,C),(G1,C1)),
  G1=H,
  retractall((H:-B)),
  M:assertz((H:-(C1,B))).


get_const(Args,Constants,Constraint):-
  maplist(constr,Args,Constants,ConstraintL),
  list2and(ConstraintL,Constraint).

constr(V,C,\+ (V = C)).

get_pred_const(do(Do0),AP0,AP):-
  (Do0= (\+ Do)->
    true
  ;
    Do=Do0
  ),
  functor(Do,F,A),
  Do=..[_|Args],
  (get_assoc(F/A,AP0,V)->
    put_assoc(F/A,AP0,[Args|V],AP)
  ;
    put_assoc(F/A,AP0,[Args],AP)
  ).


ac(do(_)).
nac(do(\+ _)).

/**
 * mc_gibbs_sample(:Query:atom,+Samples:int,-Probability:float,+Options:list) is det
 *
 * The predicate samples Query  a number of Mix+Samples (Mix is set with the options, default value 0) 
 * times.
 * The first Mix (that is set with the options, default value 0) samples are discarded (mixing time).
 * It performs Gibbs sampling: each sample is obtained from the previous one by resampling
 * a variable given the values of the variables in its Markov blanket.
 * If Query/Evidence are not ground, it considers them as existential queries.
 *
 * Options is a list of options, the following are recognised by mc_gibbs_sample/4:
 * * block(+Block:int)
 *   Perform blocked Gibbs: Block variables are sampled together, default value 1
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * successes(-Successes:int)
 *   Number of succeses
 * * failures(-Failures:int)
 *   Number of failueres
 */
mc_gibbs_sample(Goal,S,P,Options):-
  M=usermod,
  option(mix(Mix),Options,0),
  option(block(Block),Options,1),
  option(successes(T),Options,_T),
  option(failures(F),Options,_F),
  mc_gibbs_sample(M:Goal,S,Mix,Block,T,F,P).

/**
 * mc_gibbs_sample(:Query:atom,+Samples:int,-Probability:float) is det
 *
 * Equivalent to mc_gibbs_sample/4 with an empty option list.
 */
mc_gibbs_sample(Goal,S,P):-
  mc_gibbs_sample(Goal,S,P,[]).

mc_gibbs_sample(M:Goal,S,Mix,Block,T,F,P):-
  copy_term(Goal,Goal1),
  (M:Goal1->
    Succ=1
  ;
    Succ=0
  ),
  (Mix=0->
    T1=Succ,
    S1 is S-1
  ;
    T1=0,
    S1=S,
    Mix1 is Mix-1,
    gibbs_montecarlo(Mix1,0,Block,M:Goal,_TMix)
  ),
  gibbs_montecarlo(S1,T1,Block,M:Goal,T),
  P is T / S,
  F is S - T,
  erase_samples.

gibbs_montecarlo(K,T,_Block,_Goals,T):-
  K=<0,!.

gibbs_montecarlo(K0, T0,Block,M:Goal,T):-
  remove_samples(Block,LS),
  copy_term(Goal,Goal1),
  (M:Goal1->
    Succ=1
  ;
    Succ=0
  ),
  T1 is T0 + Succ,
  K1 is K0-1,
  check_sampled(M,LS),
  gibbs_montecarlo(K1,T1,Block,M:Goal,T).

remove_samples(Block,Samp):-
  remove_samp(Block,Samp).

remove_samp(0,[]):-!.

remove_samp(Block0,[(R,S)|Samp]):-
  retract(sampled(R,S,_)),!,
  Block is Block0-1,
  remove_samp(Block,Samp).

remove_samp(_,[]).


% check_sampled(M,R,S):-
%   M:sampled(R,S,_),!.

check_sampled(M,S):-
  maplist(check_sam(M),S).

check_sam(M,(R,S)):-
  M:samp(R,S,_).
/**
 * mc_gibbs_sample(:Query:atom,:Evidence:atom,+Samples:int,-Probability:float,+Options:list) is det
 *
 * The predicate samples Query  a number of Mix+Samples (Mix is set with the options, default value 0) times given that
 * Evidence
 * is true and returns
 * the number of Successes, of Failures and the
 * Probability (Successes/Samples).
 * The first Mix (that is set with the options, default value 0) samples are discarded (mixing time).
 * It performs Gibbs sampling: each sample is obtained from the previous one by resampling
 * a variable given the values of the variables in its Markov blanket.
 * If Query/Evidence are not ground, it considers them as existential queries.
 *
 * Options is a list of options, the following are recognised by mc_gibbs_sample/5:
 * * block(+Block:int)
 *   Perform blocked Gibbs: Block variables are sampled together, default value 1
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * successes(-Successes:int)
 *   Number of succeses
 * * failures(-Failures:int)
 *   Number of failueres
 */
mc_gibbs_sample(Goal,Evidence,S,P,Options):-
  M=usermod,
  option(mix(Mix),Options,0),
  option(block(Block),Options,1),
  option(successes(T),Options,_T),
  option(failures(F),Options,_F),
  mc_gibbs_sample(M:Goal,M:Evidence,S,Mix,Block,T,F,P).


mc_gibbs_sample(M:Goal,M:Evidence0,S,Mix,Block,T,F,P):-
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  gibbs_sample_cycle(M:Evidence),
  copy_term(Goal,Goal1),
  (M:Goal1->
    Succ=1
  ;
    Succ=0
  ),
  (Mix=0->
    T1=Succ,
    S1 is S-1
  ;
    T1=0,
    S1=S,
    Mix1 is Mix-1,
    gibbs_montecarlo(Mix1,0,Block,M:Goal,M:Evidence,_TMix)
  ),
  gibbs_montecarlo(S1,T1,Block,M:Goal,M:Evidence,T),
  P is T / S,
  F is S - T,
  erase_samples,
  maplist(retract,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).

gibbs_montecarlo(K,T,_Block,_G,_Ev,T):-
  K=<0,!.

gibbs_montecarlo(K0, T0,Block, M:Goal, M:Evidence,  T):-
  remove_samples(Block,LS),
  save_samples_copy(M,Evidence),
  gibbs_sample_cycle(M:Evidence),
  delete_samples_copy(M,Evidence),
  copy_term(Goal,Goal1),
  (M:Goal1->
    Succ=1
  ;
    Succ=0
  ),
  T1 is T0 + Succ,
  K1 is K0-1,
  check_sampled(M,LS),
  gibbs_montecarlo(K1, T1,Block,M:Goal,M:Evidence, T).


/**
 * mc_gibbs_sample_arg(:Query:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query  a number of Samples times.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * The first Mix (that is set with the options, default value 0) samples are discarded (mixing time).
 * It performs Gibbs sampling: each sample is obtained from the previous one by resampling
 * a variable given the values of the variables in its Markov blanket.
 *
 * Options is a list of options, the following are recognised by mc_gibbs_sample_arg/5:
 * * block(+Block:int)
 *   Perform blocked Gibbs: Block variables are sampled together, default value 1
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with
 *   a bar for each possible value of L,
 *   the list of value of Arg for which Query succeeds in
 *   a world sampled at random.
 */
mc_gibbs_sample_arg(Goal,S,Arg,ValList,Options):-
  M=usermod,
  option(mix(Mix),Options,0),
  option(block(Block),Options,1),
  option(bar(Chart),Options,no),
  mc_gibbs_sample_arg0(M:Goal,S,Mix,Block,Arg,ValList),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).
/**
 * mc_gibbs_sample_arg(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * Equivalent to mc_gibbs_sample_arg/5 with an empty option list.
 */
mc_gibbs_sample_arg(Goal,S,Arg,ValList):-
  mc_gibbs_sample_arg(Goal,S,Arg,ValList,[]).
/**
 * mc_gibbs_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * The first Mix (that is set with the options, default value 0) samples are discarded (mixing time).
 * It performs Gibbs sampling: each sample is obtained from the previous one by resampling
 * a variable given the values of the variables in its Markov blanket.
 *
 * Options is a list of options, the following are recognised by mc_gibbs_sample_arg/6:
 * * block(+Block:int)
 *   Perform blocked Gibbs: Block variables are sampled together, default value 1
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with
 *   a bar for each possible value of L,
 *   the list of value of Arg for which Query succeeds in
 *   a world sampled at random.
 */
mc_gibbs_sample_arg(Goal,Evidence,S,Arg,ValList,Options):-
  M=usermod,
  option(mix(Mix),Options,0),
  option(block(Block),Options,1),
  option(bar(Chart),Options,no),
  mc_gibbs_sample_arg0(M:Goal,M:Evidence,S,Mix,Block,Arg,ValList),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).




gibbs_sample_arg(K,_Goals,_Ev,_Block,_Arg,V,V):-
  K=<0,!.

gibbs_sample_arg(K0,M:Goal, M:Evidence,Block, Arg,V0,V):-
  remove_samples(Block,LS),
  save_samples_copy(M,Evidence),
  gibbs_sample_cycle(M:Evidence),
  delete_samples_copy(M,Evidence),
  findall(Arg,M:Goal,La),
  numbervars(La),
  (get_assoc(La, V0, N)->
    N1 is N+1,
    put_assoc(La,V0,N1,V1)
  ;
    put_assoc(La,V0,1,V1)
  ),
  K1 is K0-1,
  check_sampled(M,LS),
  gibbs_sample_arg(K1,M:Goal,M:Evidence,Block,Arg,V1,V).


/**
 * mc_gibbs_sample_arg0(:Query:atom,:Evidence:atom,+Samples:int,+Mix:int,+Block:int,+Lag:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * It performs blocked Gibbs sampling: each sample is obtained from the 
 * previous one by resampling
 * Block variables given the values of the variables in its Markov blanket.
 */
mc_gibbs_sample_arg0(M:Goal,M:Evidence0,S,Mix,Block,Arg,ValList):-
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  gibbs_sample_cycle(M:Evidence),
  empty_assoc(Values0),
  findall(Arg,M:Goal,La),
  numbervars(La),
  put_assoc(La,Values0,1,Values1),
  (Mix=0->
    Values2=Values1,
    S1 is S-1
  ;
    Mix1 is Mix-1,
    gibbs_sample_arg(Mix1,M:Goal,M:Evidence,Block,Arg, Values1,_Values),
    S1=S,
    Values2=Values0
  ),
  gibbs_sample_arg(S1,M:Goal,M:Evidence,Block,Arg, Values2,Values),
  erase_samples,
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList),
  maplist(retract,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).

/**
 * mc_gibbs_sample_arg0(:Query:atom,+Samples:int,+Mix:int,+Block:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query  a number of Samples times.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * It performs blocked Gibbs sampling: each sample is obtained from the previous one 
 * by resampling
 * Block variables given the values of the variables in its Markov blanket.
 */
mc_gibbs_sample_arg0(M:Goal,S,Mix,Block,Arg,ValList):-
  empty_assoc(Values0),
  findall(Arg,M:Goal,La),
  numbervars(La),
  put_assoc(La,Values0,1,Values1),
  (Mix=0->
    Values2=Values1,
    S1 is S-1
  ;
    Mix1 is Mix-1,
    gibbs_sample_arg(Mix1,M:Goal,Block,Arg, Values1,_Values),
    S1=S,
    Values2=Values0
  ),
  gibbs_sample_arg(S1,M:Goal,Block,Arg, Values2,Values),
  erase_samples,
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList).

gibbs_sample_arg(K,_Goals,_Block,_Arg,V,V):-
  K=<0,!.

gibbs_sample_arg(K0,M:Goal,Block, Arg,V0,V):-
  remove_samples(Block,LS),
  findall(Arg,M:Goal,La),
  numbervars(La),
  (get_assoc(La, V0, N)->
    N1 is N+1,
    put_assoc(La,V0,N1,V1)
  ;
    put_assoc(La,V0,1,V1)
  ),
  check_sampled(M,LS),
  K1 is K0-1,
  gibbs_sample_arg(K1,M:Goal,Block,Arg,V1,V).

/**
 * mc_mh_sample(:Query:atom,:Evidence:atom,+Samples:int,-Probability:float,+Options:list) is det
 *
 * The predicate samples Query  a number of Mix+Samples (Mix is set with the options, default value 0) times given that
 * Evidence
 * is true and returns
 * the number of Successes, of Failures and the
 * Probability (Successes/Samples).
 * The first Mix (that is set with the options, default value 0) samples are discarded (mixing time).
 * It performs Metropolis/Hastings sampling: between each sample, Lag (that is set with the options, default value 1) sampled
 * choices are forgotten and each sample is accepted with a certain probability.
 * If Query/Evidence are not ground, it considers them as existential queries.
 *
 * Options is a list of options, the following are recognised by mc_mh_sample/5:
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * lag(+Lag:int)
 *   lag between each sample, Lag sampled choices are forgotten, default value 1
 * * successes(-Successes:int)
 *   Number of succeses
 * * failures(-Failures:int)
 *   Number of failueres
 */
mc_mh_sample(Goal,Evidence,S,P,Options):-
  M=usermod,
  empty_assoc(Values0),
  option(lag(L),Options,1),
  option(mix(Mix),Options,0),
  option(successes(T),Options,_T),
  option(failures(F),Options,_F),
  mc_mh_sample(M:Goal,M:Evidence,S,Mix,L,T,F,P).

/**
 * mc_mh_sample(:Query:atom,:Evidence:atom,+Samples:int,-Probability:float) is det
 *
 * Equivalent to mc_mh_sample/5 with an empty option list.
 */
mc_mh_sample(Goal,Evidence,S,P):-
  mc_mh_sample(Goal,Evidence,S,P,[]).
  
/**
 * mc_mh_sample(:Query:atom,:Evidence:atom,+Samples:int,+Mix:int,+Lag:int,-Successes:int,-Failures:int,-Probability:float) is det
 *
 * The predicate samples Query  a number of Mix+Samples times given that
 * Evidence
 * is true and returns
 * the number of Successes, of Failures and the
 * Probability (Successes/Samples).
 * The first Mix samples are discarded (mixing time).
 * It performs Metropolis/Hastings sampling: between each sample, Lag sampled
 * choices are forgotten and each sample is accepted with a certain probability.
 * If Query/Evidence are not ground, it considers them as existential queries.
 */
mc_mh_sample(M:Goal,M:Evidence0,S,Mix,L,T,F,P):-
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  initial_sample_cycle(M:Evidence),!,
  copy_term(Goal,Goal1),
  (M:Goal1->
    Succ=1
  ;
    Succ=0
  ),
  count_samples(NC),
  Mix1 is Mix-1,
  save_samples_copy(M,Goal),
  mh_montecarlo(L,Mix1,NC,0, Succ,Succ,Succ1,M:Goal, M:Evidence, _NMix, _TMix),
  count_samples(NC1),
  mh_montecarlo(L,S,NC1,0, 0,Succ1,_Succ1, M:Goal, M:Evidence, _N, T),
  P is T / S,
  F is S - T,
  erase_samples,
  delete_samples_copy(M,Goal),
  maplist(retract,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).


gibbs_sample_cycle(M:G):-
  copy_term(G,G1),
  (M:G1->
    true
  ;
    erase_samples,
    restore_samples(M,G),
    gibbs_sample_cycle(M:G)
  ).


initial_sample_cycle(M:G):-
  copy_term(G,G1),
  (initial_sample(M:G1)->
    true
  ;
    erase_samples,
    initial_sample_cycle(M:G)
  ).

initial_sample(_M:true):-!.

initial_sample(M:(A~= B)):-!,
  add_arg(A,B,A1),
  initial_sample(M:A1).

initial_sample(M:msw(A,B)):-!,
  msw(M:A,B).

initial_sample(_M:(sample_head(R,VC,HL,NH))):-!,
  sample_head(R,VC,HL,NH).

initial_sample(_M:sample_gauss(R,VC,Mean,Variance,S)):-!,
  sample_gauss(R,VC,Mean,Variance,S).

initial_sample(_M:sample_geometric(R,VC,Par,S)):-!,
  sample_geometric(R,VC,Par,S).

initial_sample(_M:sample_dirichlet(R,VC,Par,S)):-!,
  sample_dirichlet(R,VC,Par,S).

initial_sample(_M:sample_discrete(R,VC,D,S)):-!,
  sample_discrete(R,VC,D,S).

initial_sample(_M:sample_poisson(R,VC,Lambda,S)):-!,
  sample_poisson(R,VC,Lambda,S).

initial_sample(_M:sample_binomial(R,VC,N,P,S)):-!,
  sample_binomial(R,VC,N,P,S).

initial_sample(_M:sample_beta(R,VC,Alpha,Beta,S)):-!,
  sample_beta(R,VC,Alpha,Beta,S).

initial_sample(_M:sample_gamma(R,VC,Shape,Scale,S)):-!,
  sample_gamma(R,VC,Shape,Scale,S).

initial_sample(_M:sample_uniform(R,VC,L,U,S)):-!,
  sample_uniform(R,VC,L,U,S).

initial_sample(_M:sample_exponential(R,VC,Lambda,S)):-!,
  sample_exponential(R,VC,Lambda,S).

initial_sample(_M:sample_pascal(R,VC,N,P,S)):-!,
  sample_pascal(R,VC,N,P,S).

initial_sample(M:(G1,G2)):-!,
  initial_sample(M:G1),
  initial_sample(M:G2).

initial_sample(M:(G1;G2)):-!,
  initial_sample(M:G1);
  initial_sample(M:G2).

initial_sample(M:(\+ G)):-!,
  \+ initial_sample(M:G).
%  initial_sample_neg(M:G).

initial_sample(M:findall(A,G,L)):-!,
  findall(A,initial_sample(M:G),L).

initial_sample(M:G):-
  builtin(G),!,
  M:call(G).

initial_sample(M:G):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  initial_sample(M:B).

initial_sample_neg(_M:true):-!,
  fail.

initial_sample_neg(_M:(sample_head(R,VC,HL,N))):-!,
  sample_head(R,VC,HL,NH),
  NH\=N.

initial_sample_neg(_M:sample_gauss(R,VC,Mean,Variance,S)):-!,
  sample_gauss(R,VC,Mean,Variance,S).


initial_sample_neg(M:(G1,G2)):-!,
  (initial_sample_neg(M:G1),!;
  initial_sample(M:G1),
  initial_sample_neg(M:G2)).

initial_sample_neg(M:(G1;G2)):-!,
  initial_sample_neg(M:G1),
  initial_sample_neg(M:G2).

initial_sample_neg(M:(\+ G)):-!,
  initial_sample(M:G).

initial_sample_neg(M:G):-
  builtin(G),!,
  \+ M:call(G).

initial_sample_neg(M:G):-
  findall(B,M:clause(G,B),L),
  initial_sample_neg_all(L,M).

initial_sample_neg_all([],_M).

initial_sample_neg_all([H|T],M):-
  initial_sample_neg(M:H),
  initial_sample_neg_all(T,M).

check(R,VC,N):-
  sampled(R,VC,N),!.

check(R,VC,N):-
  \+ sampled(R,VC,_N),
  assertz(sampled(R,VC,N)).

check_neg(R,VC,_LN,N):-
  sampled(R,VC,N1),!,
  N\=N1.

check_neg(R,VC,LN,N):-
  \+ sampled(R,VC,_N),
  member(N1,LN),
  N1\= N,
  assertz(sampled(R,VC,N1)).

listN(0,[]):-!.

listN(N,[N1|T]):-
  N1 is N-1,
  listN(N1,T).

/**
 * mc_sample_arg(:Query:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query a number of Samples times.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 *
 * Options is a list of options, the following are recognised by mc_sample_arg/5:
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with
 *   a bar for each possible value of L,
 *   the list of value of Arg for which Query succeeds in
 *   a world sampled at random.
 */
mc_sample_arg(Goal,S,Arg,ValList,Options):-
  M=usermod,
  empty_assoc(Values0),
  sample_arg(S,M:Goal,Arg, Values0,Values),
  erase_samples,
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList),
  option(bar(Chart),Options,no),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).
/**
 * mc_sample_arg(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * Equivalent to mc_sample_arg/5 with an empty option list.
 */
mc_sample_arg(Goal,S,Arg,ValList):-
  mc_sample_arg(Goal,S,Arg,ValList,[]).

/**
 * mc_rejection_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query a number of Samples times given that
 * Evidence is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * Rejection sampling is performed.
 *
 * Options is a list of options, the following are recognised by mc_rejection_sample_arg/6:
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with
 *   a bar for each possible value of L,
 *   the list of value of Arg for which Query succeeds in
 *   a world sampled at random.
 */
mc_rejection_sample_arg(Goal,Evidence0,S,Arg,ValList,Options):-
  M=usermod,
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  empty_assoc(Values0),
  rejection_sample_arg(S,M:Goal,M:Evidence,Arg, Values0,Values),
  erase_samples,
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList),
  maplist(retract,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd),
  option(bar(Chart),Options,no),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).

/**
 * mc_rejection_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * Equivalent to mc_rejection_sample_arg/6 with an empty option list.
 */
mc_rejection_sample_arg(Goal,Evidence0,S,Arg,ValList):-
  mc_rejection_sample_arg(Goal,Evidence0,S,Arg,ValList,[]).

/**
 * mc_mh_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * The first Mix (that is set with the options, default value 0) samples are discarded (mixing time).
 * It performs Metropolis/Hastings sampling: between each sample, Lag (that is set with the options, default value 1) sampled
 * choices are forgotten and each sample is accepted with a certain probability.
 *
 * Options is a list of options, the following are recognised by mc_mh_sample_arg/6:
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * lag(+Lag:int)
 *   lag between each sample, Lag sampled choices are forgotten, default value 1
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with
 *   a bar for each possible value of L,
 *   the list of value of Arg for which Query succeeds in
 *   a world sampled at random.
 */
mc_mh_sample_arg(Goal,Evidence,S,Arg,ValList,Options):-
  M=usermod,
  option(mix(Mix),Options,0),
  option(lag(L),Options,1),
  option(bar(Chart),Options,no),
  mc_mh_sample_arg0(M:Goal,M:Evidence,S,Mix,L,Arg,ValList),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).

/**
 * mc_mh_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * Equivalent to mc_mh_sample_arg/6 with an empty option list.
 */
mc_mh_sample_arg(Goal,Evidence,S,Arg,ValList):-
  mc_mh_sample_arg(Goal,Evidence,S,Arg,ValList,[]).
  
/**
 * mc_mh_sample_arg0(:Query:atom,:Evidence:atom,+Samples:int,+Mix:int,+Lag:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples L-N where
 * L is the list of values of Arg for which Query succeeds in
 * a world sampled at random and N is the number of samples
 * returning that list of values.
 * It performs Metropolis/Hastings sampling: between each sample, Lag sampled
 * choices are forgotten and each sample is accepted with a certain probability.
 */
mc_mh_sample_arg0(M:Goal,M:Evidence0,S,Mix,L,Arg,ValList):-
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  initial_sample_cycle(M:Evidence),!,
  empty_assoc(Values0),
  findall(Arg,M:Goal,La),
  numbervars(La),
  put_assoc(La,Values0,1,Values1),
  count_samples(NC),
  Mix1 is Mix-1,
  save_samples_copy(M,Goal),
  mh_sample_arg(L,Mix1,NC,M:Goal,M:Evidence,Arg, La,La1,Values1,_Values),
  count_samples(NC1),
  mh_sample_arg(L,S,NC1,M:Goal,M:Evidence,Arg, La1,_La,Values0,Values),
  erase_samples,
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList),
  delete_samples_copy(M,Goal),
  maplist(retract,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).



mh_sample_arg(_L,K,_NC0,_Goals,_Ev,_Arg,AP,AP,V,V):-
  K=<0,!.

mh_sample_arg(L,K0,NC0,M:Goal, M:Evidence, Arg,AP0,AP,V0,V):-
  resample(L),
  copy_term(Evidence,Ev1),
  (M:Ev1->
    findall(Arg,M:Goal,La),
    numbervars(La),
    count_samples(NC1),
    (accept(NC0,NC1)->
     (get_assoc(La, V0, N)->
        N1 is N+1,
        put_assoc(La,V0,N1,V1)
      ;
        put_assoc(La,V0,1,V1)
      ),
      delete_samples_copy(M,Goal),
      save_samples_copy(M,Goal),
      K1 is K0-1,
      AP1 = La
    ;
      (get_assoc(AP0, V0, N)->
        N1 is N+1,
        put_assoc(AP0,V0,N1,V1)
      ;
        put_assoc(AP0,V0,1,V1)
      ),
      K1 is K0-1,
      AP1=AP0,
      erase_samples,
      restore_samples(M,Goal)
    )
  ;
    K1 = K0,
    NC1 = NC0,
    V1 = V0,
    AP1=AP0,
    erase_samples,
    restore_samples(M,Goal)
  ),
  mh_sample_arg(L,K1,NC1,M:Goal,M:Evidence,Arg,AP1,AP,V1,V).


rejection_sample_arg(0,_Goals,_Ev,_Arg,V,V):-!.

rejection_sample_arg(K1, M:Goals,M:Ev,Arg,V0,V):-
  erase_samples,
  copy_term(Ev,Ev1),
  (M:Ev1->
    copy_term((Goals,Arg),(Goals1,Arg1)),
    findall(Arg1,M:Goals1,L),
    numbervars(L),
    (get_assoc(L, V0, N)->
      N1 is N+1,
      put_assoc(L,V0,N1,V1)
    ;
      put_assoc(L,V0,1,V1)
    ),
    K2 is K1-1
  ;
    V1=V0,
    K2=K1
  ),
  rejection_sample_arg(K2,M:Goals,M:Ev,Arg,V1,V).

sample_arg(0,_Goals,_Arg,V,V):-!.

sample_arg(K1, M:Goals,Arg,V0,V):-
  erase_samples,
  copy_term((Goals,Arg),(Goals1,Arg1)),
  findall(Arg1,M:Goals1,L),
  numbervars(L),
  (get_assoc(L, V0, N)->
    N1 is N+1,
    put_assoc(L,V0,N1,V1)
  ;
    put_assoc(L,V0,1,V1)
  ),
  K2 is K1-1,
  sample_arg(K2,M:Goals,Arg,V1,V).

/**
 * mc_particle_sample(:Query:atom,:Evidence:list,+Samples:int,-Prob:float) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true. Evidence is a list of goals.
 * The predicate returns in Prob the probability that the query is true.
 * It performs particle filtering with likelihood weighting:
 * each sample is weighted by the
 * likelihood of an element of the Evidence list and constitutes a particle.
 * After weighting, particles are resampled and the next element of Evidence
 * is considered.
 */
mc_particle_sample(Goal,Evidence,S,P):-
  M=usermod,
  M:asserta(('$goal'(1):-Goal,!),Ref1),
  M:asserta('$goal'(0),Ref0),
  mc_particle_sample_arg('$goal'(A),Evidence,S,A,ValList),
  foldl(agg_val,ValList,0,Sum),
  foldl(value_cont_single,ValList,0,SumTrue),
  P is SumTrue/Sum,
  erase(Ref1),
  erase(Ref0).

/**
 * mc_particle_sample_arg(:Query:atom,+Evidence:list,+Samples:int,?Arg:term,-Values:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * It performs particle filtering with likelihood weighting:
 * each sample is weighted by the
 * likelihood of an element of the Evidence list and constitutes a particle.
 * After weighting, particles are resampled and the next element of Evidence
 * is considered.
 * Arg should be a variable in Query. Evidence is a list of goals.
 * Query can be either a single goal or a list of goals.
 * When Query is a single goal, the predicate returns in Values
 * a list of couples V-W where V is a value of Arg for which Query succeeds in
 * a particle in the last set of particles and W is the weight of the particle.
 * For each element of Evidence, the particles are obtained by sampling Query
 * in each current particle and weighting the particle by the likelihood
 * of the evidence element.
 * When Query is a list of goals, Arg is a list of variables, one for
 * each query of Query and Arg and Query must have the same length of Evidence.
 * Values is then list of the same length of Evidence and each of its
 * elements is a list of couples V-W where
 * V is a value of the corresponding element of Arg for which the corresponding
 * element of Query succeeds in a particle and W is the weight of the particle.
 * For each element of Evidence, the particles are obtained by sampling the
 * corresponding element of Query in each current particle and weighting
 * the particle by the likelihood of the evidence element.
 */
mc_particle_sample_arg(Goal,Evidence,S,Arg,[V0|ValList]):-
  M=usermod,
  Goal=[G1|GR],!,
  Evidence=[Ev1|EvR],
  Arg=[A1|AR],
  particle_sample_first_gl(0,S,M:G1,M:Ev1,A1,V0),
  particle_sample_arg_gl(M:GR,M:EvR,AR,1,S,ValList),
  retractall(M:mem(_,_,_,_)),
  retractall(M:mem(_,_,_,_,_)),
  retractall(M:value_particle(_,_,_)),
  retractall(M:weight_particle(_,_,_)).

mc_particle_sample_arg(Goal,Evidence,S,Arg,ValList):-
  M=usermod,
  Evidence=[Ev1|EvR],
  particle_sample_first(0,S,M:Goal,M:Ev1,Arg),
  particle_sample_arg(M:EvR,M:Goal,1,S,ValList0),
  foldl(agg_val,ValList0,0,Sum),
  Norm is S/Sum,
  retractall(M:mem(_,_,_,_)),
  retractall(M:mem(_,_,_,_,_)),
  retractall(M:value_particle(_,_,_)),
  retractall(M:weight_particle(_,_,_)),
  maplist(norm(Norm),ValList0,ValList).

/**
 * mc_particle_expectation(:Query:atom,:Evidence:atom,+N:int,?Arg:var,-Exp:float) is det
 *
 * The predicate computes the expected value of Arg in Query given Evidence by
 * particle filtering.
 * It uses N particle and sums up the weighted value of Arg for
 * each particle. The overall sum is divided by the sum of weights to give Exp.
 * Arg should be a variable in Query.
 */
mc_particle_expectation(Goal,Evidence,S,Arg,E):-
  mc_particle_sample_arg(Goal,Evidence,S,Arg,ValList),
  average(ValList,E).

particle_sample_arg_gl(M:[],M:[],[],I,_S,[]):- !,
  retractall(M:mem(I,_,_,_,_)).

particle_sample_arg_gl(M:[HG|TG],M:[HE|TE],[HA|TA],I,S,[HV|TV]):-
  I1 is I+1,
  resample_gl(M,I,I1,S),
  retractall(M:value_particle(I,_,_)),
  retractall(M:weight_particle(I,_,_)),
  particle_sample_gl(0,S,M:HG,M:HE,HA,I1,HV),
  particle_sample_arg_gl(M:TG,M:TE,TA,I1,S,TV).

resample_gl(M,I,I1,S):-
  get_values(M,I,V0),
  foldl(agg_val,V0,0,Sum),
  Norm is 1.0/Sum,
  maplist(norm(Norm),V0,V1),
  numlist(1,S,L),
  maplist(weight_to_prob,L,V1,V2),
  W is 1.0/S,
  take_samples_gl(M,0,S,I,I1,W,V2),
  retractall(M:mem(I,_,_,_,_)).

weight_to_prob(I,_V-W,I:W).

take_samples_gl(_M,S,S,_I,_I1,_W,_V):-!.

take_samples_gl(M,S0,S,I,I1,W,V):-
  S1 is S0+1,
  discrete(V,SInd),
  restore_samples(M,I,SInd),
  save_samples_tab(M,I1,S1),
  take_samples_gl(M,S1,S,I,I1,W,V).

particle_sample_gl(K,K,M:_G,_Ev,_A,I,L):-!,
  get_values(M,I,L0),
  foldl(agg_val,L0,0,Sum),
  Norm is K/Sum,
  maplist(norm(Norm),L0,L).


particle_sample_gl(K0,S,M:Goal,M:Evidence,Arg,I,L):-
  K1 is K0+1,
  restore_samples(M,I,K1),
  copy_term((Goal,Arg),(Goal1,Arg1)),
  lw_sample_cycle(M:Goal1),
  copy_term(Evidence,Ev1),
  lw_sample_weight_cycle(M:Ev1,W),
  save_samples_tab(M,I,K1),
  assert(M:weight_particle(I,K1,W)),
  assert(M:value_particle(I,K1,Arg1)),
  particle_sample_gl(K1,S,M:Goal,M:Evidence,Arg,I,L).

particle_sample_first_gl(K,K,M:_Goals,_Ev,_Arg,L):-!,
  get_values(M,1,L0),
  foldl(agg_val,L0,0,Sum),
  Norm is K/Sum,
  maplist(norm(Norm),L0,L).


particle_sample_first_gl(K0,S,M:Goal, M:Evidence, Arg,V):-
  K1 is K0+1,
  copy_term((Goal,Arg),(Goal1,Arg1)),
  lw_sample_cycle(M:Goal1),
  copy_term(Evidence,Ev1),
  lw_sample_weight_cycle(M:Ev1,W),
  save_samples_tab(M,1,K1),
  assert(M:weight_particle(1,K1,W)),
  assert(M:value_particle(1,K1,Arg1)),
  particle_sample_first_gl(K1,S,M:Goal,M:Evidence,Arg,V).


particle_sample_arg(M:[],_Goal,I,_S,L):-!,
  get_values(M,I,L).

particle_sample_arg(M:[HE|TE],M:Goal,I,S,L):-
  I1 is I+1,
  resample(M,I,I1,S),
  retractall(M:value_particle(I,_,_)),
  retractall(M:weight_particle(I,_,_)),
  particle_sample(0,S, M:HE, I1),
  retractall(M:mem(I,_,_,_,_)),
  particle_sample_arg(M:TE,M:Goal,I1,S,L).

resample(M,I,I1,S):-
  get_values(M,I,V0),
  foldl(agg_val,V0,0,Sum),
  Norm is 1.0/Sum,
  maplist(norm(Norm),V0,V1),
  numlist(1,S,L),
  maplist(weight_to_prob,L,V1,V2),
  W is 1.0/S,
  take_samples(M,0,S,I,I1,W,V2).


take_samples(_M,S,S,_I,_I1,_W,_V):-!.

take_samples(M,S0,S,I,I1,W,V):-
  S1 is S0+1,
  discrete(V,SInd),
  restore_samples(M,I,SInd),
  save_samples_tab(M,I1,S1),
  M:value_particle(I,SInd,Arg),!,
  assert(M:value_particle(I1,S1,Arg)),
  take_samples(M,S1,S,I,I1,W,V).


particle_sample(K,K,_Ev,_I):-!.

particle_sample(K0,S,M:Evidence,I):-
  K1 is K0+1,
  restore_samples(M,I,K1),
  copy_term(Evidence,Ev1),
  lw_sample_weight_cycle(M:Ev1,W),
  save_samples_tab(M,I,K1),
  assert(M:weight_particle(I,K1,W)),
  particle_sample(K1,S,M:Evidence,I).

particle_sample_first(K,K,_Goals,_Ev,_Arg):-!.

particle_sample_first(K0,S,M:Goal, M:Evidence, Arg):-
  K1 is K0+1,
  copy_term((Goal,Arg),(Goal1,Arg1)),
  lw_sample_cycle(M:Goal1),
  copy_term(Evidence,Ev1),
  lw_sample_weight_cycle(M:Ev1,W),
  save_samples_tab(M,1,K1),
  assert(M:weight_particle(1,K1,W)),
  assert(M:value_particle(1,K1,Arg1)),
  particle_sample_first(K1,S,M:Goal,M:Evidence,Arg).

get_values(M,I,V):-
  findall(A-W,(M:value_particle(I,S,A),M:weight_particle(I,S,W)),V).

/**
 * mc_lw_sample(:Query:atom,:Evidence:atom,+Samples:int,-Prob:float) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * The predicate returns in Prob the probability that the query is true.
 * It performs likelihood weighting: each sample is weighted by the
 * likelihood of evidence in the sample.
 */
mc_lw_sample(Goal,Evidence0,S,P):-
  M=usermod,
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  erase_samples,
  lw_sample_bool(S,M:Goal,M:Evidence,ValList),
  foldl(agg_val,ValList,0,Sum),
  foldl(value_cont_single,ValList,0,SumTrue),
  P is SumTrue/Sum,
  maplist(retract,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).


value_cont_single(H-W,S,S+H*W).


/**
 * mc_lw_sample_arg(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples V-W where
 * V is a value of Arg for which Query succeeds in
 * a world sampled at random and W is the weight of the sample.
 * It performs likelihood weighting: each sample is weighted by the
 * likelihood of evidence in the sample.
 */
mc_lw_sample_arg(Goal,Evidence0,S,Arg,ValList):-
  M=usermod,
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  lw_sample_arg(S,M:Goal,M:Evidence,Arg,ValList0),
  foldl(agg_val,ValList0,0,Sum),
  Norm is S/Sum,
  maplist(norm(Norm),ValList0,ValList),
  maplist(retract,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).

/**
 * mc_lw_sample_arg_log(:Query:atom,:Evidence:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query  a number of Samples times given that Evidence
 * is true.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples V-W where
 * V is a value of Arg for which Query succeeds in
 * a world sampled at random and W is the natural logarithm of the weight of \
 * the sample.
 * It performs likelihood weighting: each sample is weighted by the
 * likelihood of evidence in the sample.
 * It differs from mc_lw_sample_arg/5 because the natural logarithm of the
 * weight is returned, useful when the evidence is very unlikely.
 */
mc_lw_sample_arg_log(Goal,Evidence0,S,Arg,ValList):-
  M=usermod,
  deal_with_ev(Evidence0,M,Evidence,UpdatedClausesRefs,ClausesToReAdd),
  lw_sample_arg_log(S,M:Goal,M:Evidence,Arg,ValList),
  maplist(retract,UpdatedClausesRefs),
  maplist(M:assertz,ClausesToReAdd).

/**
 * mc_lw_expectation(:Query:atom,:Evidence:atom,+N:int,?Arg:var,-Exp:float) is det
 *
 * The predicate computes the expected value of Arg in Query given Evidence by
 * likelihood weighting.
 * It takes N samples of Query and sums up the weighted value of Arg for
 * each sample. The overall sum is divided by the sum of weights to give Exp.
 * Arg should be a variable in Query.
 */
mc_lw_expectation(Goal,Evidence,S,Arg,E):-
  mc_lw_sample_arg(Goal,Evidence,S,Arg,ValList),
  average(ValList,E).



norm(NF,V-W,V-W1):-
  W1 is W*NF.

lw_sample_bool(0,_Goals,_Ev,[]):-!.

lw_sample_bool(K0,M:Goal, M:Evidence, [S-W|V]):-
  copy_term(Goal,Goal1),
  (lw_sample(M:Goal1)->
    S=1
  ;
    S=0
  ),
  copy_term(Evidence,Ev1),
  (lw_sample_weight(M:Ev1,1,W0)->
    W=W0
  ;
    W=0
  ),
  K1 is K0-1,
  erase_samples,
  lw_sample_bool(K1,M:Goal,M:Evidence,V).

lw_sample_arg(0,_Goals,_Ev,_Arg,[]):-!.

lw_sample_arg(K0,M:Goal, M:Evidence, Arg,[Arg1-W|V]):-
  copy_term((Goal,Arg),(Goal1,Arg1)),
  lw_sample_cycle(M:Goal1),
  copy_term(Evidence,Ev1),
  lw_sample_weight_cycle(M:Ev1,W),
  K1 is K0-1,
  erase_samples,
  lw_sample_arg(K1,M:Goal,M:Evidence,Arg,V).

lw_sample_cycle(M:G):-
  (lw_sample(M:G)->
    true
  ;
    erase_samples,
    lw_sample_cycle(M:G)
  ).

lw_sample_arg_log(0,_Goals,_Ev,_Arg,[]):-!.

lw_sample_arg_log(K0,M:Goal, M:Evidence, Arg,[Arg1-W|V]):-
  copy_term((Goal,Arg),(Goal1,Arg1)),
  lw_sample_cycle(M:Goal1),
  copy_term(Evidence,Ev1),
  lw_sample_logweight_cycle(M:Ev1,W),
  K1 is K0-1,
  erase_samples,
  lw_sample_arg_log(K1,M:Goal,M:Evidence,Arg,V).


lw_sample(_M:true):-!.

lw_sample(M:A~=B):-!,
  add_arg(A,B,A1),
  lw_sample(M:A1).

lw_sample(M:msw(A,B)):-!,
  msw(M:A,B).

lw_sample(M:G):-
  G=..[call,P|A],!,
  G1=..[P|A],
  lw_sample(M:G1).

lw_sample(_M:(sample_head(R,VC,HL,N))):-!,
  sample_head(R,VC,HL,N).

lw_sample(_M:sample_gauss(R,VC,Mean,Variance,S)):-!,
  sample_gauss(R,VC,Mean,Variance,S).

lw_sample(_M:sample_poisson(R,VC,Lambda,S)):-!,
  sample_poisson(R,VC,Lambda,S).

lw_sample(_M:sample_binomial(R,VC,N,P,S)):-!,
  sample_binomial(R,VC,N,P,S).

lw_sample(_M:sample_beta(R,VC,Alpha,Beta,S)):-!,
  sample_beta(R,VC,Alpha,Beta,S).

lw_sample(_M:sample_gamma(R,VC,Shape,Scale,S)):-!,
  sample_gamma(R,VC,Shape,Scale,S).

lw_sample(_M:sample_dirichlet(R,VC,Par,S)):-!,
  sample_dirichlet(R,VC,Par,S).

lw_sample(_M:sample_geometric(R,VC,Par,S)):-!,
  sample_geometric(R,VC,Par,S).

lw_sample(_M:sample_uniform(R,VC,L,U,S)):-!,
  sample_uniform(R,VC,L,U,S).

lw_sample(_M:sample_exponential(R,VC,Lambda,S)):-!,
  sample_exponential(R,VC,Lambda,S).

lw_sample(_M:sample_pascal(R,VC,N,P,S)):-!,
  sample_pascal(R,VC,N,P,S).

%lw_sample(_M:sample_discrete(R,VC,D,S)):-!,
%  sample_discrete(R,VC,D,S).

lw_sample(_M:sample_discrete(R,VC,D,S)):-!,
  sample_head(R,VC,D,SN),
  nth0(SN,D,S:_P).

lw_sample(M:(G1,G2)):-!,
  lw_sample(M:G1),
  lw_sample(M:G2).

lw_sample(M:(G1;G2)):-!,
  lw_sample(M:G1);
  lw_sample(M:G2).

lw_sample(M:(\+ G)):-!,
  \+ lw_sample(M:G).

lw_sample(M:G):-
  builtin(G),!,
  M:call(G).

lw_sample(_:G):-
  \+ \+ sampled_g(G),!,
  sampled_g(G).

lw_sample(M:G):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  lw_sample(M:B),
  assert(sampled(r,G,1)).


lw_sample_weight_cycle(M:G,W):-
  (lw_sample_weight(M:G,1,W)->
    true
  ;
    erase_samples,
    lw_sample_weight_cycle(M:G,W)
  ).

lw_sample_weight(_M:true,W,W):-!.

lw_sample_weight(M:A~= B,W0,W):-!,
  add_arg(A,B,A1),
  lw_sample_weight(M:A1,W0,W).

lw_sample_weight(M:G,W0,W):-
  G=..[call,P|A],!,
  G1=..[P|A],
  lw_sample_weight(M:G1,W0,W).

lw_sample_weight(M:msw(A,B),W0,W):-!,
  (var(B)->
    msw(M:A,B),
    W=W0
  ;
    msw_weight(M:A,B,W1),
    W is W0*W1
  ).

lw_sample_weight(_M:(sample_head(R,VC,HL,N)),W0,W):-!,
  check(R,VC,N),
  nth0(N,HL,_:P),
  W is W0*P.

lw_sample_weight(_M:sample_discrete(R,VC,D,S),W0,W):-!,
  sample_head(R,VC,D,SN),
  nth0(SN,D,S:P),
  W is W0*P.


lw_sample_weight(_M:sample_uniform(R,VC,L,U,S),W0,W):-!,
  (var(S)->
    sample_uniform(R,VC,L,U,S),
    W=W0
  ;
    uniform_density(L,U,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_gauss(R,VC,Mean,Variance,S),W0,W):-!,
  (is_var(S)->
    sample_gauss(R,VC,Mean,Variance,S),
    W=W0
  ;
    gauss_density(Mean,Variance,S,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_poisson(R,VC,Lambda,S),W0,W):-!,
  (var(S)->
    sample_poisson(R,VC,Lambda,S),
    W=W0
  ;
    poisson_prob(Lambda,S,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_binomial(R,VC,N,P,S),W0,W):-!,
  (var(S)->
    sample_binomial(R,VC,N,P,S),
    W=W0
  ;
    binomial_prob(N,P,S,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_gamma(R,VC,Shape,Scale,S),W0,W):-!,
  (var(S)->
    sample_gamma(R,VC,Shape,Scale,S),
    W=W0
  ;
    gamma_density(Shape,Scale,S,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_beta(R,VC,Alpha,Beta,S),W0,W):-!,
  (var(S)->
    sample_beta(R,VC,Alpha,Beta,S),
    W=W0
  ;
    beta_density(Alpha,Beta,S,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_dirichlet(R,VC,Par,S),W0,W):-!,
  (var(S)->
    sample_dirichlet(R,VC,Par,S),
    W=W0
  ;
    dirichlet_density(Par,S,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_geometric(R,VC,Par,S),W0,W):-!,
  (var(S)->
    sample_geometric(R,VC,Par,S),
    W=W0
  ;
    geometric_density(Par,S,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_exponential(R,VC,Lambda,S),W0,W):-!,
  (var(S)->
    sample_exponential(R,VC,Lambda,S),
    W=W0
  ;
    exponential_prob(Lambda,S,D),
    W is W0*D
   ).

lw_sample_weight(_M:sample_pascal(R,VC,N,P,S),W0,W):-!,
  (var(S)->
    sample_pascal(R,VC,N,P,S),
    W=W0
  ;
    pascal_prob(N,P,S,D),
    W is W0*D
   ).

lw_sample_weight(M:(G1,G2),W0,W):-!,
  lw_sample_weight(M:G1,W0,W1),
  lw_sample_weight(M:G2,W1,W).

lw_sample_weight(M:(G1;G2),W0,W):-!,
  lw_sample_weight(M:G1,W0,W);
  lw_sample_weight(M:G2,W0,W).

lw_sample_weight(M:(\+ G),W0,W):-!,
  lw_sample_weight(M:G,1,W1),
  W is W0*(1-W1).

lw_sample_weight(M:G,W,W):-
  builtin(G),!,
  M:call(G).

lw_sample_weight(_:G,W0,W):-
  \+ \+ sampled_g(G,_W1),!,
  sampled_g(G,W1),
  W is W0*W1.

lw_sample_weight(M:G,W0,W):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  lw_sample_weight(M:B,1,W1),
  assert(sampled(rw,G,W1)),
  W is W0*W1.


lw_sample_logweight_cycle(M:G,W):-
  (lw_sample_logweight(M:G,0,W)->
    true
  ;
    erase_samples,
    lw_sample_logweight_cycle(M:G,W)
  ).


lw_sample_logweight(_M:true,W,W):-!.

lw_sample_logweight(M:A~= B,W0,W):-!,
  add_arg(A,B,A1),
  lw_sample_logweight(M:A1,W0,W).

lw_sample_logweight(M:msw(A,B),W0,W):-!,
  (var(B)->
    msw(M:A,B),
    W=W0
  ;
    msw_weight(M:A,B,W1),
    W is W0+log(W1)
  ).

lw_sample_logweight(_M:(sample_head(R,VC,HL,N)),W0,W):-!,
  check(R,VC,N),
  nth0(N,HL,_:P),
  W is W0+log(P).

lw_sample_logweight(_M:sample_discrete(R,VC,D,S),W0,W):-!,
  sample_head(R,VC,D,SN),
  nth0(SN,D,S:P),
  W is W0+log(P).

lw_sample_logweight(_M:sample_uniform(R,VC,L,U,S),W0,W):-!,
  (var(S)->
    sample_uniform(R,VC,L,U,S),
    W=W0
  ;
    uniform_density(L,U,D),
    W is W0+log(D)
   ).

lw_sample_logweight(_M:sample_beta(R,VC,Alpha,Beta,S),W0,W):-!,
  (var(S)->
    sample_beta(R,VC,Alpha,Beta,S),
    W=W0
  ;
    beta_density(Alpha,Beta,S,D),
    W is W0+log(D)
   ).

lw_sample_logweight(_M:sample_gauss(R,VC,Mean,Variance,S),W0,W):-!,
  (var(S)->
    sample_gauss(R,VC,Mean,Variance,S),
    W=W0
  ;
    gauss_density(Mean,Variance,S,D),
    W is W0+log(D)
   ).

lw_sample_logweight(_M:sample_gamma(R,VC,Shape,Scale,S),W0,W):-!,
  (var(S)->
    sample_gamma(R,VC,Shape,Scale,S),
    W=W0
  ;
    gamma_density(Shape,Scale,S,D),
    W is W0+log(D)
   ).

lw_sample_logweight(_M:sample_geometric(R,VC,Par,S),W0,W):-!,
  (var(S)->
    sample_geometric(R,VC,Par,S),
    W=W0
  ;
    geometric_density(Par,S,D),
    W is W0+log(D)
   ).

lw_sample_logweight(_M:sample_dirichlet(R,VC,Par,S),W0,W):-!,
  (var(S)->
    sample_dirichlet(R,VC,Par,S),
    W=W0
  ;
    dirichlet_density(Par,S,D),
    W is W0+log(D)
   ).

lw_sample_logweight(M:(G1,G2),W0,W):-!,
  lw_sample_logweight(M:G1,W0,W1),
  lw_sample_logweight(M:G2,W1,W).

lw_sample_logweight(M:(G1;G2),W0,W):-!,
  lw_sample_logweight(M:G1,W0,W);
  lw_sample_logweight(M:G2,W0,W).

lw_sample_logweight(M:(\+ G),W0,W):-!,
  lw_sample_logweight(M:G,0,W1),
  W is W0-W1.


lw_sample_logweight(M:G,W,W):-
  builtin(G),!,
  M:call(G).

lw_sample_logweight(M:G,W0,W):-
  findall((G,B),M:clause(G,B),L),
  sample_one_back(L,(G,B)),
  lw_sample_logweight(M:B,W0,W).

is_var(S):-
  var(S),!.

is_var(S):-
  maplist(var,S).

/**
 * mc_sample_arg_first(:Query:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query a number of Samples times.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples V-N where
 * V is the value of Arg returned as the first answer by Query in
 * a world sampled at random and N is the number of samples
 * returning that value.
 * V is failure if the query fails.
 *
 * Options is a list of options, the following are recognised by mc_sample_arg_first/5:
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart with
 *   with a bar for each value of Arg returned as a first answer by Query in
 *   a world sampled at random.
 *   The size of the bar is the number of samples that returned that value.
 */
mc_sample_arg_first(Goal,S,Arg,ValList,Options):-
  M=usermod,
  empty_assoc(Values0),
  sample_arg_first(S,M:Goal,Arg, Values0,Values),
  erase_samples,
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList),
  option(bar(Chart),Options,no),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).

/**
 * mc_sample_arg_first(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * Equivalent to mc_sample_arg_first/5 with an empty option list.
 */
mc_sample_arg_first(Goal,S,Arg,ValList):-
  mc_sample_arg_first(Goal,S,Arg,ValList,[]).

sample_arg_first(0,_Goals,_Arg,V,V):-!.

sample_arg_first(K1, M:Goals,Arg,V0,V):-
  erase_samples,
  copy_term((Goals,Arg),(Goals1,Arg1)),
  (M:Goals1->
    numbervars(Arg1),
    Val=Arg1
  ;
    Val=failure
  ),
  (get_assoc(Val, V0, N)->
    N1 is N+1,
    put_assoc(Val,V0,N1,V1)
  ;
    put_assoc(Val,V0,1,V1)
  ),
  K2 is K1-1,
  sample_arg_first(K2,M:Goals,Arg,V1,V).

/**
 * mc_sample_arg_one(:Query:atom,+Samples:int,?Arg:var,-Values:list,+Options:list) is det
 *
 * The predicate samples Query a number of Samples times.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of couples V-N where
 * V is a value of Arg sampled with uniform probability from those returned
 * by Query in a world sampled at random and N is the number of samples
 * returning that value.
 * V is failure if the query fails.
 *
 * Options is a list of options, the following are recognised by mc_sample_arg_one/5:
 * * bar(-BarChar:dict)
 *   BarChart is a dict for rendering with c3 as a bar chart 
 *   with a bar for each value of Arg returned by sampling with uniform
 *   probability one answer from those returned by Query in a world sampled
 *   at random.
 *   The size of the bar is the number of samples.
 */
mc_sample_arg_one(Goal,S,Arg,ValList,Options):-
  M=usermod,
  empty_assoc(Values0),
  sample_arg_one(S,M:Goal,Arg, Values0,Values),
  erase_samples,
  assoc_to_list(Values,ValList0),
  sort(2, @>=,ValList0,ValList),
  option(bar(Chart),Options,no),
  (nonvar(Chart)->
    true
  ;
    argbar(ValList,Chart)
  ).

/**
 * mc_sample_arg_one(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * Equivalent to mc_sample_arg_one/5 with an empty option list.
 */
mc_sample_arg_one(Goal,S,Arg,ValList):-
  mc_sample_arg_one(Goal,S,Arg,ValList,[]).

sample_arg_one(0,_Goals,_Arg,V,V):-!.

sample_arg_one(K1, M:Goals,Arg,V0,V):-
  erase_samples,
  copy_term((Goals,Arg),(Goals1,Arg1)),
  findall(Arg1,M:Goals1,L),
  numbervars(L),
  sample_one(L,Val),
  (get_assoc(Val, V0, N)->
    N1 is N+1,
    put_assoc(Val,V0,N1,V1)
  ;
    put_assoc(Val,V0,1,V1)
  ),
  K2 is K1-1,
  sample_arg_one(K2,M:Goals,Arg,V1,V).

sample_one([],failure):-!.

sample_one(List,El):-
  length(List,L),
  random(0,L,Pos),
  nth0(Pos,List,El).

sample_one_back([],_):-!,
  fail.

sample_one_back(List,El):-
  length(List,L),
  random(0,L,Pos),
  nth0(Pos,List,El0,Rest),
  sample_backtracking(Rest,El0,El).

sample_backtracking([],El,El):-!.

sample_backtracking(_,El,El).

sample_backtracking(L,_El,El):-
  sample_one_back(L,El).
/**
 * mc_sample_arg_raw(:Query:atom,+Samples:int,?Arg:var,-Values:list) is det
 *
 * The predicate samples Query a number of Samples times.
 * Arg should be a variable in Query.
 * The predicate returns in Values a list of values
 * of Arg returned as the first answer by Query in
 * a world sampled at random.
 * The value is failure if the query fails.
 */
mc_sample_arg_raw(Goal,S,Arg,Values):-
  M=usermod,
  sample_arg_raw(S,M:Goal,Arg,Values),
  erase_samples.

sample_arg_raw(0,_Goals,_Arg,[]):-!.

sample_arg_raw(K1, M:Goals,Arg,[Val|V]):-
  erase_samples,
  copy_term((Goals,Arg),(Goals1,Arg1)),
  (M:Goals1->
    numbervars(Arg1),
    Val=Arg1
  ;
    Val=failure
  ),
  K2 is K1-1,
  sample_arg_raw(K2,M:Goals,Arg,V).


/**
 * mc_expectation(:Query:atom,+N:int,?Arg:var,-Exp:float) is det
 *
 * The predicate computes the expected value of Arg in Query by
 * sampling.
 * It takes N samples of Query and sums up the value of Arg for
 * each sample. The overall sum is divided by N to give Exp.
 * Arg should be a variable in Query.
 */
mc_expectation(Goal,S,Arg,E):-
  M=usermod,
  sample_val(S,M:Goal,Arg, 0,Sum),
  erase_samples,
  E is Sum/S.

/**
 * mc_gibbs_expectation(:Query:atom,+N:int,?Arg:var,-Exp:float,+Options:list) is det
 *
 * The predicate computes the expected value of Arg in Query by
 * sampling.
 * It takes N samples of Query and sums up the value of Arg for
 * each sample. The overall sum is divided by N to give Exp.
 * Arg should be a variable in Query.
 * Options is a list of options, the following are recognised by mc_mh_sample_arg/6:
 * * block(+Block:int)
 *   Perform blocked Gibbs: Block variables are sampled together, default value 1
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 */
mc_gibbs_expectation(Goal,S,Arg,E,Options):-
  mc_gibbs_sample_arg(Goal,S,Arg,ValList,Options),
  average(ValList,[E]),
  erase_samples.

/**
 * mc_gibbs_expectation(:Query:atom,+N:int,?Arg:var,-Exp:float) is det
 *
 * Equivalent to mc_gibbs_expectation/5 with an empty option list.
 */
mc_gibbs_expectation(Goal,S,Arg,E):-
  mc_gibbs_expectation(Goal,S,Arg,E,[]).


/**
 * mc_rejection_expectation(:Query:atom,:Evidence:atom,+N:int,?Arg:var,-Exp:float) is det
 *
 * The predicate computes the expected value of Arg in Query by
 * sampling.
 * It takes N samples of Query and sums up the value of Arg for
 * each sample. The overall sum is divided by N to give Exp.
 * Arg should be a variable in Query.
 */
mc_rejection_expectation(Goal,Evidence,S,Arg,E):-
  mc_rejection_sample_arg(Goal,Evidence,S,Arg,ValList,[]),
  average(ValList,[E]),
  erase_samples.

/**
 * mc_gibbs_expectation(:Query:atom,:Evidence:atom,+N:int,?Arg:var,-Exp:float,+Options:list) is det
 *
 * The predicate computes the expected value of Arg in Query by
 * Gibbs sampling.
 * It takes N samples of Query and sums up the value of Arg for
 * each sample. The overall sum is divided by N to give Exp.
 * Arg should be a variable in Query.
 *
 * Options is a list of options, the following are recognised by mc_mh_expectation/6:
 * * block(+Block:int)
 *   Perform blocked Gibbs: Block variables are sampled together, default value 1
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 */
mc_gibbs_expectation(Goal,Evidence,S,Arg,E,Options):-
  mc_gibbs_sample_arg(Goal,Evidence,S,Arg,ValList,Options),
  average(ValList,[E]),
  erase_samples.

/**
 * mc_mh_expectation(:Query:atom,:Evidence:atom,+N:int,?Arg:var,-Exp:float,+Options:list) is det
 *
 * The predicate computes the expected value of Arg in Query by
 * Metropolis Hastings sampling.
 * It takes N samples of Query and sums up the value of Arg for
 * each sample. The overall sum is divided by N to give Exp.
 * Arg should be a variable in Query.
 *
 * Options is a list of options, the following are recognised by mc_mh_expectation/6:
 * * mix(+Mix:int)
 *   The first Mix samples are discarded (mixing time), default value 0
 * * lag(+Lag:int)
 *   lag between each sample, Lag sampled choices are forgotten, default value 1
 */
mc_mh_expectation(Goal,Evidence,S,Arg,E,Options):-
  mc_mh_sample_arg(Goal,Evidence,S,Arg,ValList,Options),
  average(ValList,[E]),
  erase_samples.

/**
 * mc_mh_expectation(:Query:atom,:Evidence:atom,+N:int,?Arg:var,-Exp:float) is det
 *
 * Equivalent to mc_mh_expectation/6 with an empty option list.
 */
mc_mh_expectation(Goal,Evidence,S,Arg,E):-
  mc_mh_expectation(Goal,Evidence,S,Arg,E,[]).

value_cont([]-_,0):-!.

value_cont([H|_T]-N,S,S+N*H).

sample_val(0,_Goals,_Arg,Sum,Sum):-!.

sample_val(K1, M:Goals,Arg,Sum0,Sum):-
  erase_samples,
  copy_term((Goals,Arg),(Goals1,Arg1)),
  (M:Goals1->
    Sum1 is Sum0+Arg1
  ;
    Sum1=Sum
  ),
  K2 is K1-1,
  sample_val(K2,M:Goals,Arg,Sum1,Sum).


get_next_rule_number(PName,R):-
  retract(PName:rule_n(R)),
  R1 is R+1,
  assert(PName:rule_n(R1)).


assert_all([],_M,[]).

assert_all([H|T],M,[H|TRef]):-
  assertz(M:H),
  assert_all(T,M,TRef).


retract_all([]):-!.

retract_all([H|T]):-
  erase(H),
  retract_all(T).



add_mod_arg(A,_Module,A1):-
  A=..[P|Args],
  A1=..[P|Args].
/**
 * sample_head(+R:int,+Variables:list,+HeadList:list,-HeadNumber:int) is det
 *
 * samples a head from rule R instantiated as indicated by Variables (list of
 * constants, one per variable. HeadList contains the head as a list.
 * HeadNumber is the number of the sample head.
 * Internal predicates used by the transformed input program
 */
sample_head(R,VC,_HeadList,N):-
  sampled(R,VC,NH),!,
  N=NH.

sample_head(R,VC,HeadList,N):-
  sample(HeadList,NH),
  assertz(sampled(R,VC,NH)),
  N=NH.

sample(HeadList, HeadId) :-
  random(Prob),
  sample(HeadList, 0, 0, Prob, HeadId), !.

sample([_HeadTerm:HeadProb|Tail], Index, Prev, Prob, HeadId) :-
	Succ is Index + 1,
	Next is Prev + HeadProb,
	(Prob =< Next ->
		HeadId = Index;
		sample(Tail, Succ, Next, Prob, HeadId)).

/**
 * sample_uniform(+R:int,+VC:list,+Lower:float,+Upper:float,-S:float) is det
 *
 * Returns in S a sample from a distribution uniform in (Lower,Upper)
 * associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_uniform(R,VC,_L,_U,S):-
  sampled(R,VC,S0),!,
  S=S0.

sample_uniform(R,VC,L,U,S):-
  random(V),
  S0 is L+V*(U-L),
  assertz(sampled(R,VC,S0)),
  S=S0.

uniform_density(L,U,D):-
  D is 1/(U-L).

/**
 * sample_gauss(+R:int,+VC:list,+Mean:float,+Variance:float,-S:float) is det
 *
 * Returns in S a sample for a Gaussian variable with mean Mean and variance
 * Variance associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_gauss(R,VC,_Mean,_Variance,S):-
  sampled(R,VC,S0),!,
  S=S0.

sample_gauss(R,VC,Mean,Variance,S):-
  gauss(Mean,Variance,S0),
  assertz(sampled(R,VC,S0)),
  S=S0.
/**
 * gauss(+Mean:float,+Variance:float,-S:float) is det
 *
 * samples a value from a Gaussian with mean Mean and variance
 * Variance and returns it in S
 */
gauss(Mean,Variance,S):-
  number(Mean),!,
  random(U1),
  random(U2),
  R is sqrt(-2*log(U1)),
  Theta is 2*pi*U2,
  S0 is R*cos(Theta),
  StdDev is sqrt(Variance),
  S is StdDev*S0+Mean.

gauss([Mean1,Mean2],Covariance,[X1,X2]):-!,
  random(U1),
  random(U2),
  R is sqrt(-2*log(U1)),
  Theta is 2*pi*U2,
  S0 is R*cos(Theta),
  S1 is R*sin(Theta),
  cholesky_decomposition(Covariance,A),
  matrix_multiply(A,[[S0],[S1]],Az),
  matrix_sum([[Mean1],[Mean2]],Az,[[X1],[X2]]).

gauss(Mean,Covariance,X):-
  length(Mean,N),
  n_gauss_var(0,N,Z),
  cholesky_decomposition(Covariance,A),
  transpose([Z],ZT),
  matrix_multiply(A,ZT,Az),
  transpose([Mean],MT),
  matrix_sum(MT,Az,XT),
  transpose(XT,[X]).

n_gauss_var(N,N,[]):-!.

n_gauss_var(N1,N,[Z]):-
  N1 is N-1,!,
  random(U1),
  random(U2),
  R is sqrt(-2*log(U1)),
  Theta is 2*pi*U2,
  Z is R*cos(Theta).

n_gauss_var(N1,N,[Z1,Z2|T]):-
  N2 is N1+2,
  random(U1),
  random(U2),
  R is sqrt(-2*log(U1)),
  Theta is 2*pi*U2,
  Z1 is R*cos(Theta),
  Z2 is R*sin(Theta),
  n_gauss_var(N2,N,T).


/**
 * gauss_density(+Mean:float,+Variance:float,+S:float,-Density:float) is det
 *
 * Computes the probability density of value S according to a Gaussian with
 * mean Mean and variance Variance and returns it in Density.
 */
gauss_density(Mean,Variance,S,D):-
  number(Mean),!,
  StdDev is sqrt(Variance),
  D0 is 1/(StdDev*sqrt(2*pi)),
  D is D0*exp(-(S-Mean)*(S-Mean)/(2*Variance)).

gauss_density(Mean,Covariance,S,D):-
  determinant(Covariance,Det),
  matrix_diff([Mean],[S],S_M),
  matrix_inversion(Covariance,Cov_1),
  transpose(S_M,S_MT),
  matrix_multiply(S_M,Cov_1,Aux),
  matrix_multiply(Aux,S_MT,[[V]]),
  length(Mean,K),
  D is 1/sqrt((2*pi)^K*Det)*exp(-V/2).

/**
 * sample_gamma(+R:int,+VC:list,+Shape:float,+Scale:float,-S:float) is det
 *
 * Returns in S a sample for a Gamma distributed variable with shape Shape and
 * scale Scale associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_gamma(R,VC,_Shape,_Scale,S):-
  sampled(R,VC,S0),!,
  S=S0.

sample_gamma(R,VC,Shape,Scale,S):-
  gamma(Shape,Scale,S0),
  assertz(sampled(R,VC,S0)),
  S=S0.

/**
 * gamma(+Shape:float,+Scale:float,-S:float) is det
 *
 * samples a value from a Gamma density function with shape Shape and
 * scale Scale returns it in S
 */
gamma(A,Scale,S):-
  (A>=1->
    gamma_gt1(A,S1)
  ;
    random(U),
    A1 is A+1,
    gamma_gt1(A1,S0),
    S1 is S0*U^(1/A)
  ),
  S is Scale*S1.

gamma_gt1(A,S):-
  D is A-1.0/3.0,
  C is 1.0/sqrt(9.0*D),
  cycle_gamma(D,C,S).

cycle_gamma(D,C,S):-
  getv(C,X,V),
  random(U),
  S0 is D*V,
  (U<1-0.0331*X^4->
    S=S0
  ;
    LogU is log(U),
    LogV is log(V),
    (LogU<0.5*X^2+D*(1-V+LogV)->
      S=S0
    ;
      cycle_gamma(D,C,S)
    )
  ).

getv(C,X,V):-
  gauss(0.0,1.0,X0),
  V0 is (1+C*X0)^3,
  (V0=<0->
    getv(C,X,V)
  ;
    V=V0,
    X=X0
  ).

gamma_density(K,Scale,S,D):-
  D is exp(-lgamma(K))/(Scale^K)*S^(K-1)*exp(-S/Scale).

/**
 * sample_beta(+R:int,+VC:list,+Alpha:float,+Beta:float,-S:float) is det
 *
 * Returns in S a sample for a beta distributed variable with parameters
 * Alpha and Beta associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_beta(R,VC,_A,_B,S):-
  sampled(R,VC,S0),!,
  S=S0.

sample_beta(R,VC,A,B,S):-
  beta(A,B,S0),
  assertz(sampled(R,VC,S0)),
  S=S0.

/**
 * beta(+Alpha:float,+Beta:float,-S:float) is det
 *
 * samples a value from a beta probability distribution with parameters
 * Alpha and Beta and returns it in S.
 * Uses the algorithm of
 * van der Waerden, B. L., "Mathematical Statistics", Springer
 * see also
 * https://en.wikipedia.org/wiki/Beta_distribution#Generating_beta-distributed_random_variates
 */
beta(Alpha,Beta,S):-
  gamma(Alpha,1,X),
  gamma(Beta,1,Y),
  S is X/(X+Y).

beta_density(Alpha,Beta,X,D):-
  B is exp(lgamma(Alpha)+lgamma(Beta)-lgamma(Alpha+Beta)),
  D is X^(Alpha-1)*((1-X)^(Beta-1))/B.


/**
 * sample_poisson(+R:int,+VC:list,+Lambda:float,-S:int) is det
 *
 * Returns in S a sample for a Poisson distributed variable with parameter
 * lambda Lambda associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_poisson(R,VC,_L,S):-
  sampled(R,VC,S0),!,
  S=S0.

sample_poisson(R,VC,L,S):-
  poisson(L,S0),
  assertz(sampled(R,VC,S0)),
  S=S0.

/**
 * poisson(+Lambda:float,-S:int) is det
 *
 * samples a value from a Poisson probability distribution with parameter
 * Lambda and returns it in S.
 * Uses the inversion by sequential search
 * Devroye, Luc (1986). "Discrete Univariate Distributions"
 * Non-Uniform Random Variate Generation. New York: Springer-Verlag. p. 505.
 */
poisson(Lambda,X):-
  P is exp(-Lambda),
  random(U),
  poisson_cycle(0,X,Lambda,P,P,U).

poisson_cycle(X,X,_L,_P,S,U):-
  U=<S,!.

poisson_cycle(X0,X,L,P0,S0,U):-
  X1 is X0+1,
  P is P0*L/X1,
  S is S0+P,
  poisson_cycle(X1,X,L,P,S,U).

poisson_prob(Lambda,X,P):-
  fact(X,1,FX),
  P is (Lambda^X)*exp(-Lambda)/FX.

fact(N,F,F):- N =< 0, !.

fact(N,F0,F):-
  F1 is F0*N,
  N1 is N-1,
  fact(N1,F1,F).

/**
 * sample_binomial(+R:int,+VC:list,+N:int,+P:float,-S:int) is det
 *
 * Returns in S a sample for a binomial distributed variable with parameters
 * N and P associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_binomial(R,VC,_N,_P,S):-
  sampled(R,VC,S0),!,
  S=S0.

sample_binomial(R,VC,N,P,S):-
  binomial(N,P,S0),
  assertz(sampled(R,VC,S0)),
  S=S0.

/**
 * binomial(+N:int,+P:float,-S:int) is det
 *
 * samples a value from a binomial probability distribution with parameters
 * N and P and returns it in S.
 */
binomial(N,1.0,N):-!.

binomial(N,P,X):-
  Pr0 is (1-P)^N,
  random(U),
  binomial_cycle(0,X,N,P,Pr0,Pr0,U).

binomial_cycle(X,X,_N,_P,_Pr,CPr,U):-
  U=<CPr,!.

binomial_cycle(X0,X,N,P,Pr0,CPr0,U):-
  X1 is X0+1,
  Pr is Pr0*P*(N-X0)/(X1*(1-P)),
  CPr is CPr0+Pr,
  binomial_cycle(X1,X,N,P,Pr,CPr,U).

binomial_prob(N,P,X,Pr):-
  fact(N,1,FN),
  fact(X,1,FX),
  N_X is N-X,
  fact(N_X,1,FN_X),
  Pr is P^X*(1-P)^N_X*FN/(FX*FN_X).

/**
 * sample_dirichlet(+R:int,+VC:list,+Par:list,-S:float) is det
 *
 * Returns in S a sample for a Dirichlet distributed variable with parameters
 * Par associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_dirichlet(R,VC,_Par,S):-
  sampled(R,VC,S0),!,
  S=S0.

sample_dirichlet(R,VC,Par,S):-
  dirichlet(Par,S0),
  assertz(sampled(R,VC,S0)),
  S=S0.

/**
 * dirichlet(+Par:list,-S:float) is det
 *
 * samples a value from a Dirichlet probability density with parameters
 * Par and returns it in S
 */
dirichlet(Par,S):-
  maplist(get_gamma,Par,Gammas),
  sum_list(Gammas,Sum),
  maplist(divide(Sum),Gammas,S).

divide(S0,A,S):-
  S is A/S0.

get_gamma(A,G):-
  gamma(A,1.0,G).

dirichlet_density(Par,S,D):-
  beta(Par,B),
  foldl(prod,S,Par,1,D0),
  D is D0*B.

prod(X,A,P0,P0*X^(A-1)).


/**
 * sample_geometric(+R:int,+VC:list,+P:float,-S:int) is det
 *
 * Returns in S a sample for a geometrically distributed variable with
 * parameter P associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_geometric(R,VC,_Par,S):-
  sampled(R,VC,S0),!,
  S=S0.

sample_geometric(R,VC,Par,S):-
  geometric(Par,S0),
  assertz(sampled(R,VC,S0)),
  S=S0.

/**
 * geometric(+P:float,-I:int) is det
 *
 * samples a value from a geometric probability distribution with parameters
 * P and returns it in I (I belongs to [1,infinity]
 */
geometric(P,I):-
  geometric_val(1,P,I).

geometric_val(N0,P,N):-
  random(R),
  (R=<P->
    N=N0
  ;
    N1 is N0+1,
    geometric_val(N1,P,N)
  ).

geometric_density(P,I,D):-
  D is ((1-P)^(I-1))*P.

/**
 * sample_discrete(+R:int,+VC:list,+Distribution:list,-S:float) is det
 *
 * Returns in S a sample from a discrete distribution Distribution (a list
 * of couples Val:Prob) associated to rule R with substitution VC.
 * If the variable has already been sampled, it retrieves the sampled
 * value, otherwise it takes a new sample and records it for rule R with
 * substitution VC.
 */
sample_discrete(R,VC,_D,S):-
  sampled(R,VC,S1),!,
  S=S1.

sample_discrete(R,VC,D,S):-
  discrete(D,S0),
  assertz(sampled(R,VC,S0)),
  S=S0.

/**
 * discrete(+Distribution:list,-S:float) is det
 *
 * samples a value from a discrete distribution Distribution (a list
 * of couples Val:Prob) and returns it in S
 */
discrete(D,S):-
  random(U),
  discrete(D,0,U,S).


discrete([S:_],_,_,S):-!.

discrete([S0:W|T],W0,U,S):-
  W1 is W0+W,
  (U=<W1->
    S=S0
  ;
    discrete(T,W1,U,S)
  ).



/**
 * sample_exponential(+R:int,+VC:list,+Lambda:float,-S:int) is det
 *
 * Returns in S a sample for a exponential distributed variable with parameters
 * Lambda associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_exponential(R,VC,_,S):-
  sampled(R,VC,S),!.

sample_exponential(R,VC,Lambda,S):-
  exponential(Lambda,S),
  assertz(sampled(R,VC,S)).


/** 
 * exponential(+Lambda:float, -V:int) is det
 *
 * Samples a value from exponential distribution with parameter Lambda 
 * 
**/
exponential(Lambda,V):-
  V0 is 1 - exp(-Lambda),
  random(RandomVal),    
  exponential_(1,RandomVal,Lambda,V0,V).

exponential_(I,RandomVal,_,CurrentProb,I):-
  RandomVal =< CurrentProb, !.
exponential_(I,RandomVal,Lambda,_,V):-
  I1 is I+1,
  CurrentProb is 1 - exp(-Lambda*I1),
  exponential_(I1,RandomVal,Lambda,CurrentProb,V).

exponential_prob(X,Lambda,V):-
  V is Lambda*exp(-Lambda*X).


/**
 * sample_pascal(+R:int,+VC:list,+N:int,+P:float,-S:int) is det
 *
 * Returns in S a sample for a pascal distributed variable with parameters
 * N and P associated to rule R with substitution VC. If the variable
 * has already been sampled, it retrieves the sampled value, otherwise
 * it takes a new sample and records it for rule R with substitution VC.
 */
sample_pascal(R,VC,_N,_P,S):-
  sampled(R,VC,S),!.

sample_pascal(R,VC,N,P,S):-
  pascal(N,P,S),
  assertz(sampled(R,VC,S)).

/**
 * pascal(+R:int,+P:float,-Value:int) is det
 *
 * samples a value from a pascal probability distribution with parameters
 * R and P and returns it in Value. 
 * R is the number of failures
 * P is the success probability
 */

% R number of failures
% P probability of success
pascal(R,P,Value):-
  pascal_prob(0,R,P,V0),
  random(RandomVal),
  pascal_prob_(0,R,P,V0,RandomVal,Value).

pascal_prob_(I,_,_,CurrentProb,RandomVal,I):-
  RandomVal =< CurrentProb, !.
pascal_prob_(I,R,P,CurrentProb,RandomVal,V):-
  I1 is I+1,
  pascal_prob(I1,R,P,V0),
  CurrentProb1 is V0 + CurrentProb,
  pascal_prob_(I1,R,P,CurrentProb1,RandomVal,V).

/*
* K number of successes
* R number of failures
* P probability of success
*/
pascal_prob(K,R,P,Value):-
  KR1 is K+R-1,
  binomial_coeff(KR1,K,Bin),
  Value is Bin*(P**K)*(1-P)**R.

binomial_coeff(N,K,Val):-
  fact(N,1,NF),
  fact(K,1,KF),
  NK is N-K,
  fact(NK,1,NKF),
  Val is NF/(KF*NKF).

generate_rules_fact([],HeadList,VC,R,_N,[Rule,(:- dynamic samp/3)]):-
  Rule=(samp(R,VC,N):-(sample_head(R,VC,HeadList,N))).

generate_rules_fact([],_HL,_VC,_R,_N,[]).

generate_rules_fact([Head:_P1,'':_P2],HeadList,VC,R,N,[Clause, (:- dynamic F/A)]):-!,
  functor(Head,F,A),
  Clause=(Head:-(sample_head(R,VC,HeadList,N))).

generate_rules_fact([Head:_P|T],HeadList,VC,R,N,[Clause, (:- dynamic F/A)|Clauses]):-
  functor(Head,F,A),
  (Clause=(Head:-(sample_head(R,VC,HeadList,N)))),
  N1 is N+1,
  generate_rules_fact(T,HeadList,VC,R,N1,Clauses).


generate_clause_samp(Head,Body,HeadList,VC,R,N,[Rule,Clause,(:- dynamic F/A),(:- dynamic samp/3)]):-
  functor(Head,F,A),
  generate_clause(Head,Body,HeadList,VC,R,N,Clause),
  Rule=(samp(R,VC,Val):-sample_head(R,VC,HeadList,Val)).

generate_clause(Head,Body,HeadList,VC,R,N,Clause):-
  Clause=(Head:-(Body,sample_head(R,VC,HeadList,N))).

generate_clause_gauss(Head,Body,VC,R,Var,Mean,Variance,Clause):-
  functor(Head,F,A),
  Clause=[(Head:-(Body,sample_gauss(R,VC,Mean,Variance,Var))),
  (samp(R,VC,Var):-sample_gauss(R,VC,Mean,Variance,Var)),(:- dynamic F/A),(:- dynamic samp/3)].

generate_clause_uniform(Head,Body,VC,R,Var,L,U,Clause):-
  functor(Head,F,A),
  Clause=[(Head:-(Body,sample_uniform(R,VC,L,U,Var))),
  (samp(R,VC,Var):-sample_uniform(R,VC,L,U,Var)),(:- dynamic F/A),(:- dynamic samp/3)].

generate_clause_gamma(H1,Body,VC,R,Shape,Scale,Var,Clause):-
  functor(H1,F,A),
      Clause=[(H1:-(Body,sample_gamma(R,VC,Shape,Scale,Var))),
      (samp(R,VC,Var):-sample_gamma(R,VC,Shape,Scale,Var)),(:- dynamic F/A),(:- dynamic samp/3)].

generate_clause_beta(H1,Body,VC,R,Alpha,Beta,Var,Clause):-
  functor(H1,F,A),
  Clause=[(H1:-(Body,sample_beta(R,VC,Alpha,Beta,Var))),
  (samp(R,VC,Var):-sample_beta(R,VC,Alpha,Beta,Var)),(:- dynamic F/A),(:- dynamic samp/3)].

generate_clause_poisson(H1,Body,VC,R,Lambda,Var,Clause):-
  functor(H1,F,A),
  Clause=[(H1:-(Body,sample_poisson(R,VC,Lambda,Var))),
  (samp(R,VC,Var):-sample_poisson(R,VC,Lambda,Var)),(:- dynamic F/A),(:- dynamic samp/3)].

generate_clause_binomial(H1,Body,VC,R,N,P,Var,Clause):-
  functor(H1,F,A),
  Clause=[(H1:-(Body,sample_binomial(R,VC,N,P,Var))),
  (samp(R,VC,Var):-sample_binomial(R,VC,N,P,Var)),(:- dynamic F/A),(:- dynamic samp/3)].

generate_clause_discrete_uniform(H1,Body,VC,R,D0,Var,Clause):-
  functor(H1,F,A),
  Clause=[(:- import maplist/3 from swi),(H1:-Body,length(D0,Len),Prob is 1.0/Len,
      maplist(add_prob(Prob),D0,D),sample_discrete(R,VC,D,Var)),
  (samp(R,VC,Var):-length(D0,Len),Prob is 1.0/Len,
      maplist(add_prob(Prob),D0,D),sample_discrete(R,VC,D,Var)),(:- dynamic F/A),(:- dynamic samp/3)].

generate_clause_discrete(H1,Body,VC,R,D,Var,Clause):-
  functor(H1,F,A),
  Clause=[(H1:-Body,sample_discrete(R,VC,D,Var)),
  (samp(R,VC,Var):-sample_discrete(R,VC,D,Var)),(:- dynamic F/A),(:- dynamic samp/3)].

generate_clause_finite(H1,Body,VC,R,D0,Var,Clause):-
  functor(H1,F,A),
  Clause=[(:- import maplist/3 from swi),(H1:-Body,maplist(swap,D0,D),sample_discrete(R,[],D,Var)),
    (samp(R,VC,Var):-maplist(swap,D0,D),sample_discrete(R,VC,D,Var)),(:- dynamic F/A),(:- dynamic samp/3)].

generate_clause_geometric(H1,Body,VC,R,Par,Var,Clause):-
  functor(H1,F,A),
  Clause=[(:- import maplist/3 from swi),(H1:-Body,sample_geometric(R,VC,Par,Var)),
    (samp(R,VC,Var):-sample_geometric(R,VC,Par,Var)),(:- dynamic F/A),(:- dynamic samp/3)].

generate_clause_dirichlet(H1,Body,VC,R,Par,Var,Clause):-
  functor(H1,F,A),
  Clause=[(H1:-Body,sample_dirichlet(R,VC,Par,Var)),
    (samp(R,VC,Var):-sample_dirichlet(R,VC,Par,Var)),(:- dynamic F/A),(:- dynamic samp/3)].


generate_rules([],_Body,HeadList,VC,R,_N,[Rule,(:- dynamic samp/3)]):-
  Rule=(samp(R,VC,N):-sample_head(R,VC,HeadList,N)).

generate_rules([Head:_P1,'':_P2],Body,HeadList,VC,R,N,[Clause,(:- dynamic F/A)]):-!,
  functor(Head,F,A),
  generate_clause(Head,Body,HeadList,VC,R,N,Clause).

generate_rules([Head:_P|T],Body,HeadList,VC,R,N,[Clause,(:- dynamic F/A)|Clauses]):-
  functor(Head,F,A),
  generate_clause(Head,Body,HeadList,VC,R,N,Clause),
  N1 is N+1,
  generate_rules(T,Body,HeadList,VC,R,N1,Clauses).






process_head(HeadList, GroundHeadList) :-
  ground_prob(HeadList), !,
  process_head_ground(HeadList, 0, GroundHeadList).

process_head(HeadList0, HeadList):-
  get_probs(HeadList0,PL),
  foldl(minus,PL,1,PNull),
  append(HeadList0,['':PNull],HeadList).

minus(A,B,B-A).

prob_ann(_:P,P):-!.
prob_ann(P::_,P).

/**
 * add_prob(?Prob:float,:Goal:atom,?AnnGoal:atom) is det
 *
 * From Prob and Goal builds the annotated atom AnnGoal=Goal:Prob.
 */
add_prob(P,A,A:P).

/* process_head_ground([Head:ProbHead], Prob, [Head:ProbHead|Null])
 * ----------------------------------------------------------------
 */
process_head_ground([H], Prob, [Head:ProbHead1|Null]) :-
  (H=Head:ProbHead;H=ProbHead::Head),!,
  ProbHead1 is ProbHead,
  ProbLast is 1 - Prob - ProbHead1,
  prolog_load_context(module, M),mc_input_mod(M),
  M:local_mc_setting(epsilon_parsing, Eps),
  EpsNeg is - Eps,
  ProbLast > EpsNeg,
  (ProbLast > Eps ->
    Null = ['':ProbLast]
  ;
    Null = []
  ).

process_head_ground([H|Tail], Prob, [Head:ProbHead1|Next]) :-
  (H=Head:ProbHead;H=ProbHead::Head),
  ProbHead1 is ProbHead,
  ProbNext is Prob + ProbHead1,
  process_head_ground(Tail, ProbNext, Next).


ground_prob([]).

ground_prob([_Head:ProbHead|Tail]) :-!,
  ground(ProbHead), % Succeeds if there are no free variables in the term ProbHead.
  ground_prob(Tail).

ground_prob([ProbHead::_Head|Tail]) :-
  ground(ProbHead), % Succeeds if there are no free variables in the term ProbHead.
  ground_prob(Tail).

get_probs(Head, PL):-
  maplist(prob_ann,Head,PL).



/**
 * set_mc(:Parameter:atom,+Value:term) is det
 *
 * The predicate sets the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
set_mc(M:Parameter,Value):-
  retract(M:local_mc_setting(Parameter,_)),
  assert(M:local_mc_setting(Parameter,Value)).

/**
 * setting_mc(:Parameter:atom,?Value:term) is det
 *
 * The predicate returns the value of a parameter
 * For a list of parameters see
 * https://github.com/friguzzi/cplint/blob/master/doc/manual.pdf or
 * http://ds.ing.unife.it/~friguzzi/software/cplint-swi/manual.html
 */
setting_mc(M:P,V):-
  M:local_mc_setting(P,V).

extract_vars_list(L,[],V):-
  rb_new(T),
  extract_vars_term(L,T,T1),
  rb_keys(T1,V).

extract_vars_term(Variable, Var0, Var1) :-
  var(Variable), !,
  (rb_lookup(Variable, Var0,_) ->
    Var1 = Var0
  ;
    rb_insert(Var0,Variable,1,Var1)
  ).

extract_vars_term(Term, Var0, Var1) :-
  Term=..[_F|Args],
  extract_vars_tree(Args, Var0, Var1).

rb_new([]).

rb_keys(A,A).

rb_lookup(E,T,_):-
  member_eq(E,T),!.

rb_insert(L,E,_,[E|L]).

member_eq(Item, [Head|_Tail]) :-
	Item==Head, !.

member_eq(Item, [_Head|Tail]) :-
	member_eq(Item, Tail).


extract_vars_tree([], Var, Var).

extract_vars_tree([Term|Tail], Var0, Var1) :-
  extract_vars_term(Term, Var0, Var),
  extract_vars_tree(Tail, Var, Var1).

delete_equal([],_,[]).

delete_equal([H|T],E,T):-
  H == E,!.

delete_equal([H|T],E,[H|T1]):-
  delete_equal(T,E,T1).

add_arg(A,Arg,A1):-
  A=..L,
  append(L,[Arg],L1),
  A1=..L1.

/**
 * set_sw(:Var:term,+List:lit) is det
 *
 * Sets the domain of the random variable Var to List.
 * This is a predicate for programs in the PRISM syntax
 */
set_sw(A,B):-
  M=usermod,
  assert(M:sw(A,B)).

/**
 * msw(:Var:term,?Value:term) is det
 *
 * Gets or tests the Value of the random variable Var.
 * This is a predicate for programs in the PRISM syntax
 */
msw(A,B):-
  M=usermod,
  M:values(A,V),
  M:sw(A,D),
  sample_msw(V,D,B).

sample_msw(real,norm(Mean,Variance),V):-!,
  gauss(Mean,Variance,S),
  S=V.

sample_msw(Values,Dist,V):-
  maplist(combine,Values,Dist,VD),
  sample(VD,N),
  nth0(N,Values,V).

combine(V,P,V:P).

msw_weight(M:A,B,W):-
  M:values(A,V),
  M:sw(A,D),
  msw_weight(V,D,B,W).

msw_weight(real,norm(Mean,Variance),V,W):-!,
  gauss_density(Mean,Variance,V,W).

msw_weight(Values,Dist,V,W):-
  maplist(combine,Values,Dist,VD),
  member(V:W,VD).

act(M,A/B):-
  M:(dynamic A/B).

tab(A/B,A/B1):-
  B1 is B + 2.


load_cpl(File):-
	atom_concat(File,'.cpl',FileCpl),
  load(FileCpl).

load_pl(File):-
	atom_concat(File,'.pl',Filepl),
  load(Filepl).

load(File):-
	atom_concat(File,'.P',FileP),
	parse(File,FileP),
  load_comp(FileP).

initialize_mc:-
  M=usermod,
  retractall(local_mc_setting(_,_)),
  findall(local_mc_setting(P,V),default_setting_mc(P,V),L),
  assert_all(L,M,_),
  assert(mc_input_mod(M)),
  retractall(M:rule_n(_)),
  assert(M:rule_n(0)),
  dynamic(M:mem/4),
  dynamic((M:mc_on/0, M:if_on/0)).

 
load_comp(File):-
  initialize_mc,
  consult(File).

  
parse(FileIn,FileOut):-
  initialize_mc,
  prolog_load_context(module, M),
	open(FileIn,read,SI),
	read_clauses(SI,C),
	close(SI),
	process_clauses(C,[],Cl0),
  divide_dyn_imp_dir(Cl0,Dyn0,Imp0,Cl),
  sort(Imp0,Imp),
  sort(Dyn0,Dyn),
	open(FileOut,write,SO),
  write_dir(Imp,SO),
  write_dir(Dyn,SO),
	write_clauses(Cl,SO),
	close(SO).

write_dir([],S):-
	nl(S).

write_dir([H|T],S):-
  copy_term(H,H1),
  numbervars(H1,0,_),
	format(S,"~w.",[H1]),
	nl(S),
	write_dir(T,S).

write_clauses([],_).

write_clauses([H|T],S):-
  copy_term(H,H1),
  numbervars(H1,0,_),
	format(S,"~q.",[H1]),
	nl(S),
	write_clauses(T,S).

read_clauses(S,[Cl|Out]):-
        read_term(S,Cl,[]),
	(Cl=end_of_file->
		Out=[]
	;
		read_clauses(S,Out)
	).


divide_dyn_imp_dir([],[],[],[]).

divide_dyn_imp_dir([(:- dynamic A)|T],[(:- dynamic A)|Dyn],Imp,Cl):-!,
  divide_dyn_imp_dir(T,Dyn,Imp,Cl).

divide_dyn_imp_dir([(:- import A)|T],Dyn,[(:- import A)|Imp],Cl):-!,
  divide_dyn_imp_dir(T,Dyn,Imp,Cl).

divide_dyn_imp_dir([H|T],Dyn,Imp,[H|Cl]):-
  divide_dyn_imp_dir(T,Dyn,Imp,Cl).

/* clause processing */
process_clauses([end_of_file],C,C).

process_clauses([H|T],C0,C1):-
	(term_expansion_mc(H,H1)->
		true
	;
		H1=H
	),
	(is_list(H1)->
		append(C0,H1,C2)
	;
		append(C0,[H1],C2)
	),
	process_clauses(T,C2,C1).

prolog_load_context(module,usermod).
pita_input_mod(usermod).

term_expansion_mc(C, []) :-
  C \= (:- endif),
  prolog_load_context(module, M),
  pita_input_mod(M),
  M:if_on,!.


term_expansion_mc((:- mcaction Conj), []) :-!,
  prolog_load_context(module, M),
  mc_input_mod(M),!,
  list2and(L,Conj),
  maplist(act(M),L).

term_expansion_mc((:- mc), []) :-!.

term_expansion_mc((:- begin_plp), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  assert(M:mc_on).

term_expansion_mc((:- end_plp), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  retractall(M:mc_on).

term_expansion_mc((:- begin_lpad), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  assert(M:mc_on).

term_expansion_mc((:- end_lpad), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  retractall(M:mc_on).

% term_expansion_mc((:- begin_plp), []) :-!.

% term_expansion_mc((:- end_plp), []) :-!.

% term_expansion_mc((:- begin_lpad), []) :-!.

% term_expansion_mc((:- end_lpad), []) :-!.

term_expansion_mc((:- use_module(_)), []) :-!.

term_expansion_mc((:- endif), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  retract(M:if_on).

term_expansion_mc((:- if(_)), []) :-
  prolog_load_context(module, M),
  pita_input_mod(M),!,
  assert(M:if_on).

term_expansion_mc((:- use_rendering(_)), []) :-!.

term_expansion_mc(values(A,B), values(A,B)) :-
  prolog_load_context(module, M),
  pita_input_mod(M),M:mc_on,!.



term_expansion_mc((:- table(Conj)), [(:- table(Conj1))]) :-!,
  prolog_load_context(module, M),
  mc_input_mod(M),!,
  list2and(L,Conj),
  maplist(tab,L,L1),
  list2and(L1,Conj1).


term_expansion_mc((Head:=Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% fact with uniform distr

  (Head=(H~uniform(L,U))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_uniform(H1,Body,[],R,Var,L,U,Clause)
  ;
    generate_clause_uniform(H1,Body,VC,R,Var,L,U,Clause)
  ).

term_expansion_mc((Head:=Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with gamma distr

  (Head=(H~gamma(Shape,Scale))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gamma(H1,Body,[],R,Shape,Scale,Var,Clause)
  ;
    generate_clause_gamma(H1,Body,VC,R,Shape,Scale,Var,Clause)
  ).

term_expansion_mc((Head:=Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with beta distr

  (Head=(H~beta(Alpha,Beta))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_beta(H1,Body,[],R,Alpha,Beta,Var,Clause)
  ;
    generate_clause_beta(H1,Body,VC,R,Alpha,Beta,Var,Clause)
  ).


term_expansion_mc((Head:=Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with poisson distr

  (Head=(H~poisson(Lambda))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_poisson(H1,Body,[],R,Lambda,Var,Clause)
  ;
    generate_clause_poisson(H1,Body,VC,R,Lambda,Var,Clause)
  ).

term_expansion_mc((Head:=Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with binomial distr

  (Head=(H~binomial(N,P))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_binomial(H1,Body,[],R,N,P,Var,Clause)
  ;
    generate_clause_binomial(H1,Body,VC,R,N,P,Var,Clause)
  ).

term_expansion_mc((Head:=Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with uniform distr

  (Head=(H~uniform(D0))),!,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_discrete_uniform(H1,Body,[],R,D0,Var,Clause)
  ;
    generate_clause_discrete_uniform(H1,Body,VC,R,D0,Var,Clause)
  ).

term_expansion_mc((Head:=Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with guassia distr

  (Head=(H~finite(D0))),!,
  add_arg(H,Var,H1),
  extract_vars_list([Head],[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_finite(H1,Body,[],R,D0,Var,Clause)
  ;
    generate_clause_finite(H1,Body,VC,R,D0,Var,Clause)
  ).

term_expansion_mc((Head:=Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with geometric distr

  (Head=(H~geometric(Par))), !,
  add_arg(H,Var,H1),
  extract_vars_list([H],[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_geometric(H1,Body,[],R,Par,Var,Clause)
  ;
    generate_clause_geometric(H1,Body,VC,R,Par,Var,Clause)
  ).

term_expansion_mc((Head:=Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with dirichlet distr

  (Head=(H~dirichlet(Par))), !,
  add_arg(H,Var,H1),
  extract_vars_list([H],[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_dirichlet(H1,Body,[],R,Par,Var,Clause)
  ;
    generate_clause_dirichlet(H1,Body,VC,R,Par,Var,Clause)
  ).

term_expansion_mc((Head:=Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with gaussian distr

  (Head=(H~gaussian(Mean,Variance))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gauss(H1,Body,[],R,Var,Mean,Variance,Clause)
  ;
    generate_clause_gauss(H1,Body,VC,R,Var,Mean,Variance,Clause)
  ).

term_expansion_mc((Head:=Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with exponential distr

  (Head=(H~exponential(Lambda))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-Body,sample_exponential(R,[],Lambda,Var))
  ;
    Clause=(H1:-Body,sample_exponential(R,VC,Lambda,Var))
  ).

term_expansion_mc((Head:=Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with pascal distr

  (Head=(H~pascal(N,P))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-Body,sample_pascal(R,[],N,P,Var))
  ;
    Clause=(H1:-Body,sample_pascal(R,VC,N,P,Var))
  ).

term_expansion_mc((Head:=Body),[(:- dynamic F/A),(H1:-Body)]) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with guassia distr

  (Head=(H~val(Var))), !,
  add_arg(H,Var,H1),
  functor(H1,F,A).

term_expansion_mc((Head:=Body),[(:- dynamic F/A),(Head:-Body)]) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,!,
  functor(Head,F,A).

term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with guassia distr

  (Head=(H:P)),
  nonvar(P),
  (Head=(H:gaussian(Mean,Variance))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gauss(H1,Body,[],R,Var,Mean,Variance,Clause)
  ;
    generate_clause_gauss(H1,Body,VC,R,Var,Mean,Variance,Clause)
  ).


term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% fact with uniform distr

  (Head = (_:P)),
  nonvar(P),
  (Head=(H:uniform(Var,L,U))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_uniform(H,Body,[],R,Var,L,U,Clause)
  ;
    generate_clause_uniform(H,Body,VC,R,Var,L,U,Clause)
  ).

term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with gamma distr

  (Head = (_:P)),
  nonvar(P),
  (Head=(H:gamma(Var,Shape,Scale))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gamma(H,Body,[],R,Shape,Scale,Var,Clause)
  ;
    generate_clause_gamma(H,Body,VC,R,Shape,Scale,Var,Clause)
  ).

term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with beta distr

  (Head = (_:P)),
  nonvar(P),
  (Head=(H:beta(Var,Alpha,Beta))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_beta(H,Body,[],R,Alpha,Beta,Var,Clause)
  ;
    generate_clause_beta(H,Body,VC,R,Alpha,Beta,Var,Clause)
  ).


term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with poisson distr

  (Head = (_:P)),
  nonvar(P),
  (Head=(H:poisson(Var,Lambda))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_poisson(H,Body,[],R,Lambda,Var,Clause)
  ;
    generate_clause_poisson(H,Body,VC,R,Lambda,Var,Clause)
  ).

term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with binomial distr

  Head = (_:A),
  nonvar(A),
  (Head=(H:binomial(Var,N,P))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_binomial(H,Body,[],R,N,P,Var,Clause)
  ;
    generate_clause_binomial(H,Body,VC,R,N,P,Var,Clause)
  ).

term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with uniform distr

  (Head = (_:P)),
  nonvar(P),
  (Head=(H:uniform(Var,D0))),!,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_discrete_uniform(H,Body,[],R,D0,Var,Clause)
  ;
    generate_clause_discrete_uniform(H,Body,VC,R,D0,Var,Clause)
  ).

term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with discrete distr

  (Head = (_:P)),
  nonvar(P),
  ((Head=(H:discrete(Var,D)));(Head=(H:finite(Var,D)))),!,
  extract_vars_list([Head],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_discrete(H,Body,[],R,D,Var,Clause)
  ;
    generate_clause_discrete(H,Body,VC,R,D,Var,Clause)
  ).

term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with uniform discrete distr

  (Head = (_:P)),
  nonvar(P),
  (Head=(H:uniform(Var,D0))),!,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_discrete_uniform(H,Body,[],R,D0,Var,Clause)
  ;
    generate_clause_discrete_uniform(H,Body,VC,R,D0,Var,Clause)
  ).

term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with dirichlet distr

  (Head = (_:P)),
  nonvar(P),
  (Head=(H:dirichlet(Var,Par))), !,
  extract_vars_list([H],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_dirichlet(H,Body,[],R,Par,Var,Clause)
  ;
    generate_clause_dirichlet(H,Body,VC,R,Par,Var,Clause)
  ).

term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with geometriv distr

  (Head = (_:P)),
  nonvar(P),
  (Head=(H:geometric(Var,Par))), !,
  extract_vars_list([H],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_geometric(H,Body,[],R,Par,Var,Clause)
  ;
    generate_clause_geometric(H,Body,VC,R,Par,Var,Clause)
  ).

term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with gaussian distr

  (Head = (_:P)),
  nonvar(P),
  (Head=(H:gaussian(Var,Mean,Variance))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gauss(H,Body,[],R,Var,Mean,Variance,Clause)
  ;
    generate_clause_gauss(H,Body,VC,R,Var,Mean,Variance,Clause)
  ).

term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with exponential distr

  (Head = (_:P)),
  nonvar(P),
  (Head=(H:exponential(Var,Lambda))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-Body,sample_exponential(R,[],Lambda,Var))
  ;
    Clause=(H:-Body,sample_exponential(R,VC,Lambda,Var))
  ).

term_expansion_mc((Head:-Body),Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with pascal distr

  (Head = (_:A)),
  nonvar(A),
  (Head=(H:pascal(Var,N,P))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-Body,sample_pascal(R,[],N,P,Var))
  ;
    Clause=(H:-Body,sample_pascal(R,VC,N,P,Var))
  ).

term_expansion_mc((Head :- Body), Clauses):-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive clause with more than one head atom senza depth_bound
  (Head = (_;_)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list((Head :- Body),[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_rules(HeadList,Body,HeadList,[],R,0,Clauses)
  ;
    generate_rules(HeadList,Body,HeadList,VC,R,0,Clauses)
  ).

term_expansion_mc((Head :- Body), []) :-
% disjunctive clause with a single head atom con prob. 0 senza depth_bound --> la regola e' non  caricata nella teoria e non e' conteggiata in NR
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
  ((Head:-Body) \= ((term_expansion_mc(_,_) ):- _ )),
  ((Head = (_:P));Head=(P::_)),
  ground(P),
  P=:=0.0, !.




term_expansion_mc((Head :- Body), Clauses) :-
% disjunctive clause with a single head atom senza DB, con prob. diversa da 1
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
  ((Head:-Body) \= ((term_expansion_mc(_,_) ):- _ )),
  ((Head = (H:_));(Head = (_::H))), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list((Head :- Body),[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_samp(H,Body,HeadList,[],R,0,Clauses)
  ;
    generate_clause_samp(H,Body,HeadList,VC,R,0,Clauses)
  ).

term_expansion_mc((Head :- Body), [(Head :- Body),(:- dynamic F/A)]) :-
%  definite clause
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,!,
  ((Head:-Body) \= ((term_expansion_mc(_,_) ):- _ )),
  functor(Head,F,A).

term_expansion_mc(Head,Clauses) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with more than one head atom senza db
  (Head=(_;_)), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_rules_fact(HeadList,HeadList,[],R,0,Clauses)
  ;
    generate_rules_fact(HeadList,HeadList,VC,R,0,Clauses)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% fact with uniform distr

  (Head=(H~uniform(L,U))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_uniform(H1,true,[],R,Var,L,U,Clause)
  ;
    generate_clause_uniform(H1,true,VC,R,Var,L,U,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with gamma distr

  (Head=(H~gamma(Shape,Scale))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gamma(H1,true,[],R,Shape,Scale,Var,Clause)
  ;
    generate_clause_gamma(H1,true,VC,R,Shape,Scale,Var,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with beta distr

  (Head=(H~beta(Alpha,Beta))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_beta(H1,true,[],R,Alpha,Beta,Var,Clause)
  ;
    generate_clause_beta(H1,true,VC,R,Alpha,Beta,Var,Clause)
  ).


term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with poisson distr

  (Head=(H~poisson(Lambda))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_poisson(H1,true,[],R,Lambda,Var,Clause)
  ;
    generate_clause_poisson(H1,true,VC,R,Lambda,Var,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with exponential distr

  (Head=(H~exponential(Lambda))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-sample_exponential(R,[],Lambda,Var))
  ;
    Clause=(H1:-sample_exponential(R,VC,Lambda,Var))
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with pascal distr

  (Head=(H~pascal(N,P))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H1:-sample_pascal(R,[],N,P,Var))
  ;
    Clause=(H1:-sample_pascal(R,VC,N,P,Var))
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with binomial distr

  (Head=(H~binomial(N,P))), !,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_binomial(H1,true,[],R,N,P,Var,Clause)
  ;
    generate_clause_binomial(H1,true,VC,R,N,P,Var,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with uniform distr

  (Head=(H~uniform(D0))),!,
  add_arg(H,Var,H1),
  extract_vars_list(Head,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_discrete_uniform(H1,true,[],R,D0,Var,Clause)
  ;
    generate_clause_discrete_uniform(H1,true,VC,R,D0,Var,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with guassia distr

  (Head=(H~finite(D0))),!,
  add_arg(H,Var,H1),
  extract_vars_list([Head],[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_finite(H1,true,[],R,D0,Var,Clause)
  ;
    generate_clause_finite(H1,true,VC,R,D0,Var,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with gaussian distr

  (Head=(H~gaussian(Mean,Variance))), !,
  extract_vars_list([Head],[],VC),
  add_arg(H,Var,H1),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gauss(H1,true,[],R,Var,Mean,Variance,Clause)
  ;
    generate_clause_gauss(H1,true,VC,R,Var,Mean,Variance,Clause)
  ).
term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with dirichlet distr

  (Head=(H:P)),
  nonvar(P),
  (Head=(H:dirichlet(Var,Par))), !,
  extract_vars_list([H],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_dirichlet(H,true,[],R,Par,Var,Clause)
  ;
    generate_clause_dirichlet(H,true,VC,R,Par,Var,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with geometric distr

  (Head=(H:P)),
  nonvar(P),
  (Head=(H:geometric(Var,Par))), !,
  extract_vars_list([H],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_geometric(H,true,[],R,Par,Var,Clause)
  ;
    generate_clause_geometric(H,true,VC,R,Par,Var,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with guassia distr

  (Head=(H:P)),
  nonvar(P),
  (Head=(H:gaussian(Var,Mean,Variance))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gauss(H,true,[],R,Var,Mean,Variance,Clause)
  ;
    generate_clause_gauss(H,true,VC,R,Var,Mean,Variance,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% fact with uniform distr

  (Head=(H:P)),
  nonvar(P),
  (Head=(H:uniform(Var,L,U))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_uniform(H,true,[],R,Var,L,U,Clause)
  ;
    generate_clause_uniform(H,true,VC,R,Var,L,U,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with gamma distr

  (Head=(H:P)),
  nonvar(P),
  (Head=(H:gamma(Var,Shape,Scale))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gamma(H,true,[],R,Shape,Scale,Var,Clause)
  ;
    generate_clause_gamma(H,true,VC,R,Shape,Scale,Var,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with beta distr

  (Head=(H:P)),
  nonvar(P),
  (Head=(H:beta(Var,Alpha,Beta))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_beta(H,true,[],R,Alpha,Beta,Var,Clause)
  ;
    generate_clause_beta(H,true,VC,R,Alpha,Beta,Var,Clause)
  ).


term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with poisson distr

  (Head=(H:P)),
  nonvar(P),
  (Head=(H:poisson(Var,Lambda))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_poisson(H,true,[],R,Lambda,Var,Clause)
  ;
    generate_clause_poisson(H,true,VC,R,Lambda,Var,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with binomial distr

  (Head=(H:A)),
  nonvar(A),
  (Head=(H:binomial(Var,N,P))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_binomial(H,true,[],R,N,P,Var,Clause)
  ;
    generate_clause_binomial(H,true,VC,R,N,P,Var,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with guassia distr

  (Head=(H:P)),
  nonvar(P),
  (Head=(H:uniform(Var,D0))),!,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_discrete_uniform(H,true,[],R,D0,Var,Clause)
  ;
    generate_clause_discrete_uniform(H,true,VC,R,D0,Var,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with guassia distr

  (Head=(H:P)),
  nonvar(P),
  ((Head=(H:discrete(Var,D)));(Head=(H:finite(Var,D)))),!,
  extract_vars_list([Head],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_discrete(H,true,[],R,D,Var,Clause)
  ;
    generate_clause_discrete(H,true,VC,R,D,Var,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with dirichlet distr

  (Head=(H:P)),
  nonvar(P),
  (Head=(H:dirichlet(Var,Par))), !,
  extract_vars_list([H],[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_dirichlet(H,true,[],R,Par,Var,Clause)
  ;
    generate_clause_dirichlet(H,true,VC,R,Par,Var,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with gaussian distr

  (Head=(H:P)),
  nonvar(P),
  (Head=(H:gaussian(Var,Mean,Variance))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_gauss(H,true,[],R,Var,Mean,Variance,Clause)
  ;
    generate_clause_gauss(H,true,VC,R,Var,Mean,Variance,Clause)
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with exponential distr

  (Head=(H:P)),
  nonvar(P),
  (Head=(H:exponential(Var,Lambda))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-sample_exponential(R,[],Lambda,Var))
  ;
    Clause=(H:-sample_exponential(R,VC,Lambda,Var))
  ).

term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with pascal distr

  (Head=(H:A)),
  nonvar(A),
  (Head=(H:pascal(Var,N,P))), !,
  extract_vars_list(Head,[],VC0),
  delete_equal(VC0,Var,VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    Clause=(H:-sample_pascal(R,[],N,P,Var))
  ;
    Clause=(H:-sample_pascal(R,VC,N,P,Var))
  ).

term_expansion_mc(Head,[]) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with a single head atom con prob. 0

  ((Head = (_:P)); (Head = (P::_))),
  ground(P),
  P=:=0.0, !.


term_expansion_mc(Head,H) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with a single head atom con prob. 1, senza db

  (Head = (H:P);(Head = (P::H))),
  ground(P),
  P=:=1.0, !.


term_expansion_mc(Head,Clause) :-
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
% disjunctive fact with a single head atom e prob. generiche, senza db

  ((Head=(H:_));(Head=(_::H))), !,
  list2or(HeadListOr, Head),
  process_head(HeadListOr, HeadList),
  extract_vars_list(HeadList,[],VC),
  get_next_rule_number(M,R),
  (M:local_mc_setting(single_var,true)->
    generate_clause_samp(H,true,HeadList,[],R,0,Clause)
  ;
    generate_clause_samp(H,true,HeadList,VC,R,0,Clause)
  ).

term_expansion_mc(Head, [Head ,(:- dynamic F/A)]) :-
%  definite clause
  prolog_load_context(module, M),mc_input_mod(M),M:mc_on,
  functor(Head,F,A).


/**
 * begin_lpad_pred is det
 *
 * Initializes LPAD loading.
 */
begin_lpad_pred:-
  assert(mc_input_mod(user)),
  assert(user:mc_on).

/**
 * end_lpad_pred is det
 *
 * Terminates the cplint inference module.
 */
end_lpad_pred:-
  retractall(mc_input_mod(_)),
  retractall(user:mc_on).

list2or([],true):-!.

list2or([X],X):-
    (X\= (_;_)),!.

list2or([H|T],(H ; Ta)):-!,
    list2or(T,Ta).


list2and([],true):-!.

list2and([X],X):-
    (X\= (_,_)),!.

list2and([H|T],(H,Ta)):-!,
    list2and(T,Ta).

transform(H,H1):-
  H=..[prob|Args],
  H1=..[prob_meta|Args].



append(LL,L):-
  append_int(LL,[],L).

append_int([],L,L).

append_int([H|T],L0,L):-
  append(L0,H,L1),
  append_int(T,L1,L).

nonvar_list([]).

nonvar_list([H|T]):-
  nonvar(H),
  nonvar_list(T).
/**
 * builtin(+Goal:atom) is det
 *
 * Succeeds if Goal is an atom whose predicate is defined in Prolog
 * (either builtin or defined in a standard library).
 */
builtin(average(_L,_Av)):-!.

builtin(A):-
  functor(A,between,3),!.
% *** for Teri:
% builtin(between(_,_,_)):-!. was not working
builtin(A):-
  functor(A,{},1),!.

builtin(G):-
  builtin_int(G),!.

builtin_int(average(_L,_Av)).
builtin_int(mc_prob(_,_,_)).
builtin_int(mc_prob(_,_)).
builtin_int(mc_sample(_,_,_,_)).
builtin_int(maplist(_,_,_)).
builtin_int(maplist(_,_,_,_)).
builtin_int(numlist(_,_,_)).
builtin_int(db(_)).
builtin_int(G):-
  predicate_property(G,built_in).
builtin_int(G):-
  predicate_property(G,imported_from(lists)).
builtin_int(G):-
  predicate_property(G,imported_from(apply)).
builtin_int(G):-
  predicate_property(G,imported_from(nf_r)).
builtin_int(G):-
  predicate_property(G,imported_from(matrix)).
builtin_int(G):-
  predicate_property(G,imported_from(clpfd)).




/**
 * swap(?Term1:term,?Term2:term) is det
 *
 * If Term1 is of the form A:B, then Term2 is of the form B:A.
 */
swap(A:B,B:A).

/**
 * ~=(:Term:term, +B:term) is det
 *
 * equality predicate for distributional clauses
 */
A ~= B :-
  M=usermod,
  A=..L,
  append(L,[B],L1),
  A1=..L1,
  M:A1.


remove_duplicates(L0,L):-
  remove_duplicates(L0,[],L1),
  reverse(L1,L).

remove_duplicates([],L,L).

remove_duplicates([H|T],L0,L):-
  member(H,L0),!,
  remove_duplicates(T,L0,L).

remove_duplicates([H|T],L0,L):-
  remove_duplicates(T,[H|L0],L).

sort(2, @>=,ValList0,ValList):-
  maplist(exchange,ValList0,V0),
  sort(V0,V1),
  reverse(V1,V2),
  maplist(exchange,V2,ValList).

exchange(A-B,B-A).

argbar(A,A).


/**
 * beta(+Alphas:list,-Beta:float) is det
 *
 * Computes the value of the multivariate beta function for vector Alphas
 * https://en.wikipedia.org/wiki/Beta_function#Multivariate_beta_function
 * Alphas is a list of floats
 */
beta(Par,B):-
  sum_list(Par,Sum),
  maplist(comp_lgamma,Par,LnGPar),
  LnG is lgamma(Sum),
  sum_list([-LnG|LnGPar],Exp),
  B is exp(Exp).

comp_lgamma(X,LnG):-
  LnG is lgamma(X).

/**
 * average(+Values:list,-Average:float) is det
 *
 * Computes the average of Values.
 * Values can be
 * 
 * * a list of numbers
 * * a list of couples number-weight, in which case each number is multiplied by the weight
 *   before being summed
 * * a list of lists,  in which case lists are considered as matrices of numbers and averaged
 *   element-wise
 * * a list of couples list-weight, in which case the list is considered as a matrix of numbers. 
 *   The matrix in each element of List must have the same dimension and are aggregated element-
 *   wise
 */
average([H|T],Av):-
  number(H),!,
  sum_list([H|T],Sum),
  length([H|T],N),
  Av is Sum/N.

average([H|T],E):-
  is_list(H),!,
  length(H,N),
  list0(N,L0),
  foldl(vector_sum,[H|T],L0,Sum),
  length([H|T],NV),
  matrix_div_scal([Sum],NV,[E]).

average([H-W|T],E):-
  is_list(H),!,
  length(H,N),
  list0(N,L0),
  foldl(single_value_vect,[H-W|T],L0,Sum),
  foldl(agg_val,[H-W|T],0,SW),
  matrix_div_scal([Sum],SW,[E]).

average(ValList,E):-
  foldl(single_value_cont,ValList,0,Sum),
  foldl(agg_val,ValList,0,SW),
  E is Sum/SW.


single_value_cont(H-N,S,S+N*H).

single_value_vect(H-N,S0,S):-
  matrix_mult_scal([H],N,[H1]),
  matrix_sum([H1],[S0],[S]).


agg_val(_ -N,S,O):-
  O is S+N.

vector_sum(A,B,C):-
  matrix_sum([A],[B],[C]).

/**
 * variance(+Values:list,-Average:float,-Variance:float) is det.
 * variance(+Values:list,-Variance:float) is det.
 * std_dev(+Values:list,-Average:float,-Dev:float) is det.
 * std_dev(+Values:list,-Dev:float) is det.
 *
 * Computes the variance or standard deviation (and the average) of Values.
 * Values can be
 * 
 * * a list of numbers
 * * a list of couples number-weight, in which case each number is multiplied by the weight
 *   before being considered
 * * a list of couples list-weight, in which case list is considered as a matrix of numbers. 
 *   The matrix in each element of List must have the same dimension and are aggregated element-
 *   wise
 */
variance(L,Var):-
  variance(L,_Av,Var).

variance(L,Av,Var):-
  average(L,Av),
  maplist(sq_diff(Av),L,LS), 
  average(LS,Var).

std_dev(L,Dev):-
  std_dev(L,_Av,Dev).
  
std_dev(L,Av,Dev):-
  variance(L,Av,Var),
  root(Var,Dev).

root(Var,Dev):-
  number(Var),!,
  Dev is sqrt(Var).

root(Var,Dev):-
  maplist(sqroot,Var,Dev).

sqroot(A,B):-
  B is sqrt(A).

sq_diff(Av,A,S):-
  number(A),!,
  S is (A-Av)^2.

sq_diff(Av,A-W,S):-
  number(A),!,
  S is W*(A-Av)^2.

sq_diff(Av,A-W,S):-
  maplist(sq_diff,Av,A,S0),
  matrix_mult_scal([S0],W,[S]).


/** <module> matrix

This module performs matrix operations.
Impemented operations:
 - sum
 - difference
 - multiplication
 - Cholesky decomposition https://en.wikipedia.org/wiki/Cholesky_decomposition
 - determinant for positive semi-definite matrices (using Cholesky decomposition)
 - inversion for positive semi-definite matrices (using Cholesky decomposition)
 - inversion for lower triangular matrices

The library was developed for dealing with multivariate Gaussian distributions,
that's the reson for the focus on positive semi-definite matrices

@author Fabrizio Riguzzi
@license Artistic License 2.0
@copyright Fabrizio Riguzzi
*/


% :- module(matrix,
%     [matrix_multiply/3,
%     matrix_sum/3,
%     matrix_diff/3,
%     matrix_mult_scal/3,
%     matrix_div_scal/3,
%     dot_product/3,
%     cholesky_decomposition/2,
%     matrix_inversion/2,
%     matrix_inv_triang/2,
%     determinant/2,
%     list0/2
%     ]).
% :- use_module(library(clpfd), [transpose/2]).
%%  matrix_div_scal(+A,+V,-B) is det.
% divide matrix A by scalar V
%
matrix_div_scal(A,V,B):-
  maplist(maplist_int(div(V)),A,B).

maplist_int(P,A,B):-
  maplist(P,A,B).

maplist_int(P,A,B,C):-
  maplist(P,A,B,C).

div(A,B,C):-
  C is B/A.
%%  matrix_mult_scal(+A,+V,-B) is det.
% multiply matrix A by scalar V
%
matrix_mult_scal(A,V,B):-
  maplist(maplist_int(multiplication(V)),A,B).

multiplication(A,B,C):-
  C is A*B.
%%  determinant(+A,-D) is det.
% computes the determinant for a positive semi-definite matrix.
% Uses the Cholenski decomposition
% ==
% ?- determinant([[2,-1,0],[-1,2,-1],[0,-1,2]],D).
% D = 3.999999999999999.
% ==
determinant(A,Det):-
  cholesky_decomposition(A,L),
  get_diagonal(L,D),
  foldl(prod,D,1,DetL),
  Det is DetL*DetL.
 
prod(A,P0,P):- 
  P is P0*A.

%%  matrix_inversion(+M,-IM) is det.
% inversion of a positive semi-definite matrix. Uses the Cholenski 
% decomposition
% ==
% ?- matrix_inversion([[2,-1,0],[-1,2,-1],[0,-1,2]],L).
% L = [[0.7499999999999999, 0.5000000000000001, 0.2500000000000001], [0.5000000000000001, 1.0000000000000004, 0.5000000000000002], [0.2500000000000001, 0.5000000000000002, 0.7500000000000001]].
% ==
matrix_inversion(A,B):-
  cholesky_decomposition(A,L),
  matrix_inv_triang(L,LI),
  transpose(LI,LIT),
  matrix_multiply(LIT,LI,B).

%%  matrix_inv_triang(+M,-IM) is det.
% inversion of a lower triangular matrix
% code from
% http://www.mymathlib.com/c_source/matrices/linearsystems/unit_lower_triangular.c
% http://www.mcs.csueastbay.edu/~malek/TeX/Triangle.pdf
% code from
% ==
% ?- matrix_inv_triang([[2,0,0],[-1,2,0],[0,-1,2]],L).
% L = [[0.5, 0.0, 0.0], [0.25, 0.5, 0.0], [0.125, 0.25, 0.5]].
% ==
matrix_inv_triang(L1,L2):-
  get_diagonal(L1,D),
  maplist(inv,D,ID),
  diag(ID,IDM),
  matrix_multiply(IDM,L1,LL1),
  list_to_term(LL1,LT),
  length(LL1,N),
  matrix_inv_i(1,N,LT),
  term_to_list(LT,N,LL2),
  matrix_multiply(LL2,IDM,L2).


matrix_inv_i(N,N,_LT):-!.

matrix_inv_i(I,N,LT):-
  matrix_inv_j(0,I,N,LT),
  I1 is I+1,
  matrix_inv_i(I1,N,LT).

matrix_inv_j(I,I,_N,_LT):-!.

matrix_inv_j(J,I,N,LT):-
  get_v(I,J,N,LT,Vij),
  V_ij is -Vij,
  set_v(I,J,N,LT,V_ij),
  J1 is J+1,
  matrix_inv_k(J1,J,I,N,LT),
  matrix_inv_j(J1,I,N,LT).
  
matrix_inv_k(I,_J,I,_N,_LT):-!.

matrix_inv_k(K,J,I,N,LT):-
  get_v(I,K,N,LT,Vik),
  get_v(K,J,N,LT,Vkj),
  get_v(I,J,N,LT,Vij),
  NVij is Vij-Vik*Vkj,
  set_v(I,J,N,LT,NVij),
  K1 is K+1,
  matrix_inv_k(K1,J,I,N,LT).

 
diag(L,D):-
  length(L,N),
  NN is N*N,
  list0(NN,L0),
  DT =..[a|L0],
  N1 is N-1,
  numlist(0,N1,In),
  maplist(set_diag(DT,N),In,L),
  term_to_list(DT,N,D).

set_diag(D,N,I,V):-
  set_v(I,I,N,D,V).

inv(A,B):-
  B is 1.0/A.

get_diagonal(L,D):-
  length(L,N),
  list_to_term(L,LT),
  get_diag(0,N,LT,D).

get_diag(N,N,_L,[]):-!.

get_diag(N0,N,L,[H|R]):-
  get_v(N0,N0,N,L,H),
  N1 is N0+1,
  get_diag(N1,N,L,R).

list_to_term(L,LT):-
  append(L,LL),
  LT=..[a|LL].
%%  matrix_multiply(+X,+Y,-M) is det.
%
%   X(N*P),Y(P*M),M(N*M)
% ==
% ?- matrix_multiply([[1,2],[3,4],[5,6]], [[1,1,1],[1,1,1]],R).
% R = [[3, 3, 3], [7, 7, 7], [11, 11, 11]].
% ==
% code from http://stackoverflow.com/questions/34206275/matrix-multiplication-with-prolog
matrix_multiply(X,Y,M) :-
  matrix_mul(X,Y,M0),
  maplist(maplist_int(is),M,M0).

matrix_mul(X,Y,M) :-
    transpose(Y,T),
    maplist(row_multiply(T),X,M).

row_multiply(T,X,M) :-
    maplist(dot_product(X),T,M).

%%  dot_product(+X,+Y,-D) is det.
% computes the dot produce of two vectors
%
dot_product([X|Xs],[T|Ts],M) :-
    foldl(mul,Xs,Ts,X*T,M).
mul(X,T,M,M+X*T).

%% matrix_diff(+A,+B,-C) is det
matrix_diff(X,Y,S):-
  maplist(maplist_int(sum),X,Y,S).

diff(A,B,C):-
  C is A-B.
%% matrix_sum(+A,+B,-C) is det
% ==
% matrix_sum([[1,2],[3,4],[5,6]],[[1,2],[3,4],[5,6]],M).
% ==
matrix_sum(X,Y,S):-
  maplist(maplist_int(sum),X,Y,S).

sum(A,B,C):-
  C is A+B.

%% cholesky_decomposition(+A,-L) is det.
% computes the Cholesky decomposition of a positive semi-definite matrix
% code from https://rosettacode.org/wiki/Cholesky_decomposition#C
% ==
% cholesky_decomposition([[25, 15, -5], [15, 18,  0], [-5,  0, 11]],L).
% L = [[5.0, 0, 0], [3.0, 3.0, 0], [-1.0, 1.0, 3.0]].
% cholesky_decomposition([[18, 22,  54,  42],[22, 70,  86,  62],[ 54, 86, 174, 134],[ 42, 62, 134, 106]],L).
% L = [[4.242640687119285, 0, 0, 0], [5.185449728701349, 6.565905201197403, 0, 0], [12.727922061357857, 3.0460384954008553, 1.6497422479090704, 0], [9.899494936611667, 1.624553864213788, 1.8497110052313648, 1.3926212476456026]].
% ==
cholesky_decomposition(A,L):-
  append(A,AL),
  length(AL,NL),
  AM =..[a|AL],
  list0(NL,LL),
  LM=..[l|LL],
  length(A,N),
  cholesky_i(0,N,AM,LM),
  term_to_list(LM,N,L).

cholesky_i(N,N,_A,_L):-!.

cholesky_i(I,N,A,L):-
  cholesky_j(0,I,N,A,L),
  I1 is I+1,
  cholesky_i(I1,N,A,L).

cholesky_j(I,I,N,A,L):-!,
 cholesky_k(0,I,I,N,0,S,L),
  get_v(I,I,N,A,Aii),
  V is sqrt(Aii-S),
  set_v(I,I,N,L,V).


cholesky_j(J,I,N,A,L):-
  cholesky_k(0,J,I,N,0,S,L),
  get_v(I,J,N,A,Aij),
  get_v(J,J,N,L,Ljj),
  V is 1.0/Ljj*(Aij-S),
  set_v(I,J,N,L,V),
  J1 is J+1,
  cholesky_j(J1,I,N,A,L).

cholesky_k(J,J,_I,_N,S,S,_L):-!.

cholesky_k(K,J,I,N,S0,S,L):-
  get_v(I,K,N,L,Lik),
  get_v(J,K,N,L,Ljk),
  S1 is S0+Lik*Ljk,
  K1 is K+1,
  cholesky_k(K1,J,I,N,S1,S,L).

get_v(I,J,N,M,V):-
  Argij is I*N+J+1,
  arg(Argij,M,V).

set_v(I,J,N,M,V):-
  Argij is I*N+J+1,
  setarg(Argij,M,V).

 
term_to_list(T,N,L):-
  T=..[_|E],
  identify_rows(E,N,L).

identify_rows([],_N,[]):-!.

identify_rows(E,N,[R|L]):-
  length(R,N),
  append(R,Rest,E),
  identify_rows(Rest,N,L).

%% list0(+N,-L) is det
% returns a list of N zeros
list0(0,[]):-!.

list0(N,[0|T]):-
  N1 is N-1,
  list0(N1,T).
