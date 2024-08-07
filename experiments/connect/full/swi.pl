:- ['bmlp/lmatrix.pl'].
:- [background].

%:- set_prolog_flag(table_space, 16000000000).
%:- set_prolog_flag(stack_limit, 16000000000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mode declaration and recursive relationship
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mode(edge,[node,node]).

target(connect).

connect(A,B) :-
    edge(A,B).
connect(A,B) :-
    edge(A,C),
    connect(C,B).

:- prolog_load_context(directory,Path),init(Path).

test(M) :- assertz(computation_mode(M)),
           compile,compute.