:- use_module(bmlp).

compute :-
        init('./temp'),
        compile('experiments/path/partial/background.pl',db(edge,[node,node],_),M1),
        lm_select([c1],M1,V1),
        call_time(smp((V1,M1),_),Stats),
        get_dict(cpu,Stats,CpuT),
        writeln(CpuT),halt.