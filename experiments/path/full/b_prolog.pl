:- [background].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B-Prolog

path(A,B):-edge(A,B).
path(A,B):-edge(A,C),path(C,B).

:- table path/2.

compute :-
    cputime(Start),
    closure,
    cputime(End),
    T is (End-Start)/1000,
    writeln(T),halt.

closure:-path(_C1,_C2), fail.
closure.

