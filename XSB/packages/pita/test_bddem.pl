:- consult(bddem, [cc_opts('-Icudd-3.0.0 -Icudd-3.0.0/cudd -Icudd-3.0.0/util -fPIC -DBP_FREE -O3 -fomit-frame-pointer -Wall -g -O2')]).

:- import between/3 from basics.
:- import length/2 from basics.

main:-
    test_verb(one),
    test_verb(and),
    test_verb(or),
    test_verb(nor),
    test_verb(em),
    test_verb(one_dir),
    test_verb(one_dir1),
    test_verb(gamma),
    test_verb(gauss),
    test_verb(uniform),
    test_verb(dirichlet),
    test_verb(dirichlet1),
    test_verb(dirichlet2),
    test_verb(sdirichlet1),
    test_verb(sdirichlet2),
    test_verb(discrete).

test_verb(A):-
    writeln(A),
    test(A).


v1_0(Env,R,BDD):-
  add_var(Env,[0.4,0.6],R,V),equality(Env,V,0,BDD).

v2_0(Env,R,Val,BDD):-
  add_var(Env,[0.4,0.3,0.3],R,V),equality(Env,V,Val,BDD).

test(one):-
  init(Env),
  v1_0(Env,0,BDD),
  ret_prob(Env,BDD,P),
  end(Env),
  P=:=0.4.

test(and):-
  init(Env),
  v1_0(Env,0,BDD1),
  v1_0(Env,0,BDD2),
  and(Env,BDD1,BDD2,BDD),
  ret_prob(Env,BDD,P),
  end(Env),
  P=:=0.4*0.4.

test(or):-
  init(Env),
  v1_0(Env,0,BDD1),
  v1_0(Env,0,BDD2),
  or(Env,BDD1,BDD2,BDD),
  ret_prob(Env,BDD,P),
  end(Env),
  P=:=0.4+0.4-0.4*0.4.

test(nor):-
  init(Env),
  v1_0(Env,0,BDD1),
  v1_0(Env,0,BDD2),
  or(Env,BDD1,BDD2,BDDN),
  bdd_not(Env,BDDN,BDD),
  ret_prob(Env,BDD,P),
  end(Env),
  P=:=1-(0.4+0.4-0.4*0.4).



test(em):-
  init_em(Cont),
  ex1(Cont,BDD1),
  ex1(Cont,BDD2),
  ex2(Cont,BDD3),
  em(Cont,[2,2],[[BDD1,1.0],[BDD2,1.0],[BDD3,1.0]],0.0001,0.001,100,LL,Out),
  Out=[Par,ExP],
  writeln(LL),
  writeln(Par),
  writeln(ExP),
  end_em(Cont),
  abs(LL)<  1.0e-3.

ex1(Cont,BDD):-
  init_ex(Cont,Env),
  v1_0(Env,0,B0),
  v1_0(Env,1,B1),
  or(Env,B0,B1,BDD),
  end_ex(Cont).

ex2(Cont,BDD):-
  init_ex(Cont,Env),
  v1_0(Env,0,B00),
  v1_0(Env,1,B1),
  bdd_not(Env,B00,B0),
  or(Env,B0,B1,BDD),
  end_ex(Cont).

test(one_dir):-
  init_em(Cont),
  ex1(Cont,BDD1),
  ex1(Cont,BDD2),
  ex2(Cont,BDD3),
  initial_values(Cont,1.0),
  em(Cont,[2,2],[[BDD1,1.0],[BDD2,1.0],[BDD3,1.0]],0.0001,0.001,100,LL,Out),
  Out=[Par,ExP],
  writeln(LL),
  writeln(Par),
  writeln(ExP),
  end_em(Cont),
  abs(LL)<  1.0e-3.

test(one_dir1):-
  init_em(Cont),
  ex1(Cont,BDD1),
  ex1(Cont,BDD2),
  ex2(Cont,BDD3),
  ex3(Cont,BDD4),
  ex4(Cont,BDD5),
  initial_values(Cont,1.0),
  em(Cont,[2,2,3],[[BDD1,1.0],[BDD2,1.0],[BDD3,1.0],[BDD4,1.0],[BDD5,1.0]],0.0001,0.001,100,LL,Out),
  Out=[Par,ExP],
  writeln(LL),
  writeln(Par),
  writeln(ExP),
  end_em(Cont),
  abs(LL)<  1.0e-3.

ex3(Cont,BDD):-
  init_ex(Cont,Env),
  v2_0(Env,2,1,B00),
  v2_0(Env,2,2,B1),
  bdd_not(Env,B00,B0),
  or(Env,B0,B1,BDD),
  end_ex(Cont).

ex4(Cont,BDD):-
  init_ex(Cont,Env),
  v2_0(Env,2,0,B0),
  v2_0(Env,2,2,B1),
  or(Env,B0,B1,BDD),
  end_ex(Cont).

relatively_close_to(V,T,E):-
	TLow is T*(1-E),
	THigh is T*(1+E),
	TLow=<V,
	V=<THigh.

close_to(V,T):-
	epsilon(E),
	TLow is T-E,
	THigh is T+E,
	TLow=<V,
	V=<THigh.


average([H|T],Av):-
  sum_list([H|T],Sum),
  length([H|T],N),
  Av is Sum/N.

sum_list(L,S):-
  sum_list(L,0,S).

sum_list([],S,S).

sum_list([H|T],S0,S):-
  S1 is S0+H,
  sum_list(T,S1,S).


  
variance(L,Av,Var):-
  average(L,Av),
  sq_diff(L,Av,LS), 
  average(LS,Var).

std_dev(L,Av,Dev):-
  variance(L,Av,Var),
  root(Var,Dev).

root(Var,Dev):-
  Dev is sqrt(Var).

sq_diff([],_Av,[]).

sq_diff([A|T],Av,[S|TO]):-
  S is (A-Av)^2,
  sq_diff(T,Av,TO).

is0(0).

is1(1).

is2(2).

is3(3).

test(gamma):-
  findall(S,(between(1,10000,_),gamma_sample(1.0,2.0,S)),V),
  variance(V,M,Var),
  writeln(mean(M)),
  writeln(var(Var)),
  relatively_close_to(M,2,0.2),
  relatively_close_to(Var,4,0.2).

test(gauss):-
  findall(S,(between(1,10000,_),gauss_sample(1.0,2.0,S)),V),
  variance(V,M,Var),
  writeln(mean(M)),
  writeln(var(Var)),
  relatively_close_to(M,1,0.1),
  relatively_close_to(Var,2,0.1).


test(uniform):-
  findall(S,(between(1,10000,_),uniform_sample(S)),V),
  variance(V,M,Var),
  writeln(mean(M)),
  writeln(var(Var)),
  relatively_close_to(M,0.5,0.1),
  CV is 1.0/12,
  relatively_close_to(Var,CV,0.1).

test(dirichlet):-
  findall(S,(between(1,10000,_),dirichlet_sample([1,1,1],S)),_V).

test(dirichlet1):-
  findall(S,(between(1,10000,_),dirichlet_sample([1,1,1,1],D),discrete_sample(D,S)),V),
  check_sample(V).

test(dirichlet2):-
  findall(S,(between(1,10000,_),dirichlet_sample([2,2,2,2],D),discrete_sample(D,S)),V),
  check_sample(V).

test(sdirichlet1):-
  findall(S,(between(1,10000,_),symmetric_dirichlet_sample(1.0,4,D),discrete_sample(D,S)),V),
  check_sample(V).

test(sdirichlet2):-
  findall(S,(between(1,10000,_),symmetric_dirichlet_sample(2.0,4,D),discrete_sample(D,S)),V),
  check_sample(V).

test(discrete):-
  findall(S,(between(1,10000,_),discrete_sample([0.25,0.25,0.25,0.25],S)),V),
  check_sample(V).

check_sample(V):-
  partition(is0,V,L0),
  partition(is1,V,L1),
  partition(is2,V,L2),
  partition(is3,V,L3),
  length(L0,N0),
  length(L1,N1),
  length(L2,N2),
  length(L3,N3),
  writeln(N0),
  writeln(N1),
  writeln(N2),
  writeln(N3),
  relatively_close_to(N0,2500,0.1),
  relatively_close_to(N1,2500,0.1),
  relatively_close_to(N2,2500,0.1),
  relatively_close_to(N3,2500,0.1).

partition(_Pred,[],[]).

partition(Pred,[H|T],[H|T1]):-
    At=..[Pred,H],
    At,!,
    partition(Pred,T,T1).

partition(Pred,[_H|T],T1):-
    partition(Pred,T,T1).