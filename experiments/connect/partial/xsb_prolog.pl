:- [background].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XSB-Prolog

connect(A,B):-edge(A,B).
connect(A,B):-edge(A,C),connect(C,B).

:- table connect/2.

compute :-
    cputime(Start),
    closure,
    cputime(End),
    T is (End-Start),
    writeln(T),halt.

closure:-connect(c1,_C2), fail.
closure.