%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% XSB-Prolog

connect(A,B):-edge(A,B).
connect(A,B):-edge(A,C),connect(C,B).

:- table connect/2.

closure:-connect(C1,C2), writeln(connect(C1,C2)),fail.
closure.