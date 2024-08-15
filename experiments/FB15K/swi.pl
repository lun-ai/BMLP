:- [background].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SWI-Prolog
:- set_prolog_flag(table_space, 1600000000).

p_1(X,Y) :- contains(X,Y).
p_1(X,Y) :- contains(X,Z), p_1(Z,Y).
p_2(X,Y) :- adjoins(X,Y).
p_2(X,Y) :- adjoins(Y,X).
p_2(X,Y) :- p_1(Z,X), p_2(Z,Y).
%f(X,Y) :- location(X), location(Y), \+ p_2(X,Y).

:- table p_1/2,p_2/2 as variant.

%closure:-f(_C1,_C2), fail.
closure:-p_2(_C1,_C2), fail.
closure.

compute :-
    call_time(closure,Stats),
    get_dict(cpu,Stats,CpuT),
    writeln(CpuT),halt.