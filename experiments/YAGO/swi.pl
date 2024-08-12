:- [neighbors].

:- table route/2.

:- set_prolog_flag(table_space, 16000000000).

route(A,B):-neighbors(A,B).
route(A,B):-neighbors(A,C),route(C,B).

closure :- route(_C1,_C2),fail.
closure.

compute :-
    call_time(closure,Stats),
    get_dict(cpu,Stats,CpuT),
    writeln(CpuT),halt.