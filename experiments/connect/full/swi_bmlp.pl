:- use_module(bmlp).

compute :-
        init('./temp'),
        compile('experiments/connect/full/background.pl',db(edge,[node,node],_),M1),
        call_time(compute(rms,M1,_),Stats),
        get_dict(cpu,Stats,CpuT),
        writeln(CpuT),halt.