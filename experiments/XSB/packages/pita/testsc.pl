


:- import permutation/2 from lists.
:- import maplist/2 from swi.
:- import concat_atom/2 from string.
:- import format/2 from format.

:- [slipcover].


perm(_A,_B).

% perm(A,B):-
%   permutation(A,B).

bar(_,_).
bar1(_,_).

is_dict(_).

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
  ex(E,F),
  concat_atom(['examples/learning/',F],File),
  load_pl(File),
  findall(usermod:unit_test(E,T),test_couple(E,T),L),
  maplist(call,L).


term_expansion((:- module(_,_)),[]):-!.

term_expansion((:- use_module(_)),[]):-!.

term_expansion((:- begin_tests(N, _)),[]):-!,
  assert(tests(N)),!.

term_expansion((:- end_tests(_)),[ex(N,E)]):-!,
  retract(example(E)),
  retract(tests(N)).

term_expansion( (:- ensure_loaded(library(examples/learning/E))),[]):-!,
  assert(example(E)).


term_expansion((test(T):-Body),[test_couple(N,T),(unit_test(N,T):-Body)]):-!,
  tests(N).

:-[test_sc].