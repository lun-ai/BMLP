:- ['bmlp/lmatrix.pl'].
:- [background].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mode declaration and recursive relationship
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mode(edge,[node,node]).

marking(c2).
target(connect).

connect(A,B) :-
    edge(A,B).
connect(A,B) :-
    edge(A,C),
    connect(C,B).

:- prolog_load_context(directory,Path),init(Path).

test(M) :- assertz(computation_mode(M)),
           compile,compute.