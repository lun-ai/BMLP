:- ['src/lmatrix.pl'].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mode declaration and recursive relationship
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mode(edge,[node,node]).

marking(c1).              % for abduction hypothesis confirmation
target(connect).       % for swi prolog closure computation

% logic programs represented by the compiled matrices
connect(A,B) :-
    edge(A,B).
connect(A,B) :-
    edge(A,C),
    connect(C,B).

:- prolog_load_context(directory,Path),init(Path).

test(M) :- assertz(computation_mode(M)),
           compile,compute.