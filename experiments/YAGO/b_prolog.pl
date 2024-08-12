:- [neighbors].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% B-Prolog

route(A,B):-neighbors(A,B).
route(A,B):-neighbors(A,C),route(C,B).

:- table route/2.

compute :-
    cputime(Start),
    closure,
    cputime(End),
    T is (End-Start)/1000,
    writeln(T),halt.

closure:-route(_C1,_C2), fail.
closure.

