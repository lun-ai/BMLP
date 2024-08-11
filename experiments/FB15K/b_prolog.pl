:- [contains].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B-Prolog

locatedIn(A,B):-contains(B,A).
locatedIn(A,B):-locatedIn(A,C),locatedIn(C,B).

:- table locatedIn/2.

compute :-
    cputime(Start),
    closure,
    cputime(End),
    T is (End-Start)/1000,
    writeln(T),halt.

closure:-locatedIn(_C1,_C2), fail.
closure.

