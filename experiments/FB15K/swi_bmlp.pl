:- use_module(bmlp).

compute_(M1,M2,MN5) :-
        compute(rms,M1,M3),
        compute(transpose,M3,MT3),
        compute(transpose,M2,MT2),
        compute(add,[M2,MT2],M4),
        compute(mul,[MT3,M4],M5),
        compute(negate,M5,MN5).

compute :-
        init('./test'),
        compile('experiments/FB15K/background.pl',db(contains,[location,location],_),M1),
        compile('experiments/FB15K/background.pl',db(adjoins,[location,location],_),M2),
        call_time(compute_(M1,M2,_),Stats),
        get_dict(cpu,Stats,CpuT),
        writeln(CpuT),halt.