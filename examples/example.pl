:- use_module(bmlp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mode declaration and recursive relationship
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% logic programs represented by the compiled matrices
connect(A,B) :-
    edge(A,B).
connect(A,B) :-
    edge(A,C),
    connect(C,B).


bmlp :- % initialisation to use a path
        %   for matrix computation (default is project root)
        init,
        % compile facts in DB, edges between node pairs
        %   as matrix M1
        compile('examples/ex_p3.pl',db(edge,[node,node]),M1),
        % use repeated squaring module on M1
        %   produce M2 that is automatically loaded
        trace,
        compute(rms,[M1],[M2],[output_id='connect']),
        % print output matrix if it has been loaded
        lm_print(M2).