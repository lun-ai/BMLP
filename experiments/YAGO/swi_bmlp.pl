:- use_module(bmlp).

compute :-
        init('./test'),
        compile('experiments/connect/full/background.pl',db(edge,[node,node]),M1),
        call_time(compute(rms,M1,_),Stats),
        get_dict(cpu,Stats,CpuT),
        writeln(CpuT),halt.