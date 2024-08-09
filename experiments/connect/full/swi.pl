:- [background].

:- table connect/2.

:- set_prolog_flag(table_space, 16000000000).

connect(A,B) :-
    edge(A,B).
connect(A,B) :-
    edge(A,C),
    connect(C,B).

closure :- connect(_C1,_C2),fail.
closure.

compute :-
    call_time(closure,Stats),
    get_dict(cpu,Stats,CpuT),
    writeln(CpuT),halt.