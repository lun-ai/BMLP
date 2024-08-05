


:- import permutation/2 from lists.

:- import maplist/2 from swi.
:- import concat_atom/2 from string.
:- import format/2 from format.
:- import setrand/1 from random.

:- import member/2 from basics.
:- [mcintyre].


perm(_A,_B).

% perm(A,B):-
%   permutation(A,B).

bar(_,_).
bar(_,_,_).
bar1(_,_).

is_dict(_).
is_dict(_,_).

run(H):-
    M=usermod,
	copy_term(H,NH),
	numbervars(NH,1,_),
%	NH=(_Query,close_to('P',_Prob)),
	format("~w.~n",[NH]),
	(H=(G,R)),
	call(M:G),!,
	format("\t~w.~n~n",[G]),
	call(R).

epsilon(0.09).

close_to(V,T):-
	epsilon(E),
	TLow is T-E,
	THigh is T+E,
	TLow=<V,
	V=<THigh.

close_to(V,T,E):-
	TLow is T-E,
	THigh is T+E,
	TLow=<V,
	V=<THigh.

relative_epsilon(0.1).

relatively_close_to(V,T):-
	relative_epsilon(E),
	TLow is T*(1-E),
	THigh is T*(1+E),
	TLow=<V,
	V=<THigh.

relatively_close_to(V,T,E):-
	TLow is T*(1-E),
	THigh is T*(1+E),
	TLow=<V,
	V=<THigh.


run_tests([]).

run_tests([H|T]):-
  run_test(H),
  run_tests(T).

run_test(E):-
  concat_atom(['examples/',E],File),
  load_pl(File),
  findall(usermod:test(E,T),test_couple(E,T),L),
  maplist(call,L).


term_expansion((:- module(_,_)),[]):-!.

term_expansion((:- use_module(_)),[]):-!.

term_expansion((:- begin_tests(_, _)),[]):-!.

term_expansion((:- end_tests(_)),[]):-!,
  retract(example(E)),
  assert(ex(E)).

term_expansion((:- set_random(seed(X))),[]):-!,
	setrand(rand(X,345,1234)).

term_expansion( (:- ensure_loaded(library(examples/E))),[]):-!,
  assert(example(E)).


term_expansion((test(T):-Body),[test_couple(E,T),(test(E,T):-Body)]):-!,
  example(E).

:-[test_mc].