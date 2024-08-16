:- [background].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SWI-Prolog
:- set_prolog_flag(table_space, 10000000000).

p_1(X,Y) :- contains(X,Y).
p_1(X,Y) :- contains(X,Z), p_1(Z,Y).
p_2(X,Y) :- adjoins(X,Y).
p_2(X,Y) :- adjoins(Y,X).
p_2(X,Y) :- p_1(Z,X), p_2(Z,Y).
f(X,Y) :- location(X), location(Y), p_2(X,Y).

:- table f/2,p_1/2,p_2/2.

closure:- f(_C1,_C2), fail.
closure.

compute :-
    current_prolog_flag(table_space,A),
    writeln(A),
    call_time(closure,Stats),
    get_dict(cpu,Stats,CpuT),
    writeln(CpuT),halt.