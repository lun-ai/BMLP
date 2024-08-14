:- use_module(bmlp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mode declaration and recursive relationship
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% logic programs represented by the compiled matrices
%   connect(A,B) :-
%       edge(A,B).
%   connect(A,B) :-
%       edge(A,C),
%       connect(C,B).


ex_1 :- % initialisation
        % use a path for matrix computation (default is project root)
        init('./temp'),
        % compile facts in DB
        % edges between node pairs as matrix M1
        compile('examples/ex_p5.pl',db(edge,[node,node],_),M1),
        % use repeated squaring module on M1
        % produce M2 that is automatically loaded
        compute(rms,M1,M2,[output_id='connect']),
        % print output matrix if it has been loaded
        lm_print(M2).
ex_2 :- % initialisation
        % use a path for matrix computation (default is project root)
        init('./temp'),
        % compile facts in DB
        % edges between node pairs as matrix M1
        compile('examples/ex_p2.pl',db(edge,[node,node],_),M1),
        % query the database that contains c2 as the first argument
        % encode this query as a vector V1
        lm_select([c1],M1,V1,[output_id='ex_query']),
        % produce a vector V2 as the result of query
        compute(smp,[V1,M1],V2),
        % print output vector if it has been loaded
        lm_print(V2).
composed :-
        init('./temp'),
        compile('examples/ex_p6.pl',db(contains,[location,location],_),M1),
        compile('examples/ex_p6.pl',db(adjoins,[location,location],_),M2),
        compute(rms,M1,M3),
        lm_print(M3),
        compute(transpose,M3,MT3),
        lm_print(MT3),
        compute(addI,MT3,MIT3),
        lm_print(MIT3),
        compute(transpose,M2,MT2),
        lm_print(MT2),
        compute(add,[M2,MT2],M4),
        lm_print(M4),
        compute(mul,[MIT3,M4],M5),
        lm_print(M5),
        compute(negate,M5,MN5),
        lm_print(MN5).
