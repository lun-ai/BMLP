:- ['src/lmatrix.pl'].
:- [background].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mode declaration and recursive relationship
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mode(edge,[node,node]).

%computation_mode(ime).
computation_mode(rms).
%computation_mode(swipl).
target(connect).

connect(A,B) :-
    edge(A,B).
connect(A,B) :-
    edge(A,C),
    connect(C,B).

:- init,compile,compute.