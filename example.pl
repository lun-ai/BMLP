:- use_module(bmlp).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Examples of BMLP usage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ./bmlp/tests/ex_p2.pl
rms_ex :-
        % initialisation
        % use a path for matrix computation (default is ./temp)
        init('./temp'),
        % compile facts in DB
        % edges between node pairs as matrix M1
        compile('./bmlp/tests/ex_p0.pl',db(edge,[node,node],_),M1),
        % use repeated squaring module on M1
        % produce M2 that is automatically loaded
        rms(M1,M2,[output_name='path']),
        % print output matrix if it has been loaded
        lm_print(M2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ./bmlp/tests/ex_p2.pl
smp_ex :-
        % initialisation
        % use a path for matrix computation (default is ./temp)
        init('./temp'),
        % compile facts in DB
        % edges between node pairs as matrix M1
        compile('./bmlp/tests/ex_p0.pl',db(edge,[node,node],_),M1),
        % query the database that contains c2 as the first argument
        % encode this query as a vector V1
        lm_select([a],M1,V1,[output_name='ex_query']),
        % produce a vector V2 as the result of query
        smp((V1,M1),V2),
        % print output vector if it has been loaded
        lm_print(V2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ./bmlp/tests/ex_p6.pl
% example in Section 4.4, using the BMLP architecture in Figure 3
composed :-
        init('./temp'),
        compile('./bmlp/tests/ex_p6.pl',db(contains,[location,location],_),M1),
        compile('./bmlp/tests/ex_p6.pl',db(adjoins,[location,location],_),M2),
        rms(M1,M3,[output_name='hasPlace']),
        lm_print(M3),
        % matrix transpose
        transpose(M3,MT3),
        lm_print(MT3),
        % matrix adding identity
        addI(MT3,MIT3),
        lm_print(MIT3),
        transpose(M2,MT2),
        lm_print(MT2),
        % matrix addition
        add((M2,MT2),M4),
        lm_print(M4),
        % matrix multiplication
        mul((MIT3,M4),M5,[output_name='indirectlyPartOf']),
        lm_print(M5),
        negate(M5,MN5,[output_name='isForeign']),
        lm_print(MN5).