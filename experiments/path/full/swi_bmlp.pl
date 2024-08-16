:- use_module(bmlp).

compute :-
        init('./temp'),
        compile('experiments/path/full/background.pl',db(edge,[node,node],_),M1),
        call_time(rms(M1,_),Stats),
        get_dict(cpu,Stats,CpuT),
        writeln(CpuT),halt.