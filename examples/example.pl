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
        compile('examples/ex_p5.pl',db(edge,[node,node],_),M1),
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
        compile('examples/ex_p1.pl',db(edge,[node,node],_),M1),
        % query the database that contains c2 as the first argument
        % encode this query as a vector V1
        lm_select([c100],M1,V1,[output_id='ex_query']),
        % produce a vector V2 as the result of query
        compute(smp,[V1,M1],V2),
        % print output vector if it has been loaded
        lm_print(V2).
%composed :-
%        init('./test'),
%        compile('examples/ex_p6.pl',db(t,[set,set],_),M1),
%        compile('examples/ex_p6.pl',db(g,[set,set],_),M2),
%        non_linear(),
%        % print output matrix if it has been loaded
%        lm_print(M2).
%
%non_linear(matrix(M1,_,_,_),matrix(M2,_,_,_)) :-
%        lm_submatrix(M1,M2).
%non_linear(M1,M2,M3) :-
%        compute(rms,M1,M3),
%        compute(rms,M3,M4),
%        non-linear(M4,M2).