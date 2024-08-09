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
        init('./test'),
        % compile facts in DB
        % edges between node pairs as matrix M1
        compile('examples/ex_p3.pl',db(edge,[node,node]),M1),
        % use repeated squaring module on M1
        % produce M2 that is automatically loaded
        compute(rms,M1,M2,[output_id='connect']),
        % print output matrix if it has been loaded
        lm_print(M2).
ex_2 :- % initialisation
        % use a path for matrix computation (default is project root)
        init('./test'),
        % compile facts in DB
        % edges between node pairs as matrix M1
        compile('examples/ex_p1.pl',db(edge,[node,node]),M1),
        % query the database that contains c2 as the first argument
        % encode this query as a vector V1
        lm_select([c2],M1,V1,[output_id='ex_query']),
        % produce a vector V2 as the result of query
        compute(smp,[V1,M1],V2),
        % print output vector if it has been loaded
        lm_print(V2).
composed :-
        init,
        compile('examples/ex_p1.pl',db(edge,[node,node]),M1),
        lm_select([c2],M1,V1,[output_id='ex_query']),
        % use repeated squaring module on M1
        % produce M2 that is automatically loaded
        compute(smp,[V1,M1],M2),
        % print output matrix if it has been loaded
        lm_print(M2).