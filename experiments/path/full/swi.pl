:- [background].

:- table path/2.

:- set_prolog_flag(table_space, 16000000000).

path(A,B) :-
    edge(A,B).
path(A,B) :-
    edge(A,C),
    path(C,B).

closure :- path(_C1,_C2),fail.
closure.

compute :-
    call_time(closure,Stats),
    get_dict(cpu,Stats,CpuT),
    writeln(CpuT),halt.