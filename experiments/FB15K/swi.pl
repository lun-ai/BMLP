:- [background].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SWI-Prolog
:- set_prolog_flag(table_space, 16000000000).

p_1(X,Y) :- contains(X,Y).
p_1(X,Y) :- contains(X,Z), p_1(Z,Y).
p_2(X,Y) :- adjoins(X,Y).
p_2(X,Y) :- adjoins(Y,X).
p_2(X,Y) :- p_1(Z,X), p_2(Z,Y).
f(X,Y) :- location(X), location(Y), \+ p_2(X,Y).

:- table f/2,p_1/2,p_2/2.

compute :-
    cputime(Start),
    closure,
    cputime(End),
    T is (End-Start)/1000,
    writeln(T),halt.

closure:-f(_C1,_C2), fail.
closure.