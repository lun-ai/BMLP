:- use_module(bmlp).

% program equivalent to BMLP architecture in Figure 3
%p_1(X,Y) :- contains(X,Y).
%p_1(X,Y) :- contains(X,Z), p_1(Z,Y).
%p_2(X,Y) :- adjoins(X,Y).
%p_2(X,Y) :- adjoins(Y,X).
%p_2(X,Y) :- p_1(Z,X), p_2(Z,Y).
%f(X,Y) :- location(X), location(Y), \+ p_2(X,Y).

% BMLP architecture
compute_(M1,M2,MN5) :-
        rms(M1,M3),
        transpose(M3,MT3),
        addI(MT3,MIT3),
        transpose(M2,MT2),
        add((M2,MT2),M4),
        mul((MIT3,M4),M5),
        negate(M5,MN5).

compute :-
        init('./temp'),
        compile('experiments/FB15K/background.pl',db(contains,[location,location],_),M1),
        compile('experiments/FB15K/background.pl',db(adjoins,[location,location],_),M2),
        call_time(compute_(M1,M2,_),Stats),
        get_dict(cpu,Stats,CpuT),
        writeln(CpuT),halt.