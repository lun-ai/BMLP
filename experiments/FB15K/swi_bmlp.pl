:- use_module(bmlp).

compute(M1,M2,MN5) :-
        rms(M1,M3),
        transpose(M3,MT3),
        transpose(M2,MT2),
        add([M2,MT2],M4),
        mul([MT3,M4],M5),
        negate(M5,MN5).

compute :-
        init('./temp'),
        compile('experiments/FB15K/background.pl',db(contains,[location,location],_),M1),
        compile('experiments/FB15K/background.pl',db(adjoins,[location,location],_),M2),
        call_time(compute_(M1,M2,_),Stats),
        get_dict(cpu,Stats,CpuT),
        writeln(CpuT),halt.