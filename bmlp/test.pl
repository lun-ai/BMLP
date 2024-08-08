%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   BMLP modules unit tests
%   Author: Lun Ai and S.H. Muggleton
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(random)).

:- begin_tests(max_integer_size).

foo(Numbers) :-
	concurrent_forall(
	    member(N,Numbers),
        call(foo_,N)
	).
foo_(N) :-
    lm_stob1([N],_).

% check if length-N bitcodes fit the configured integer size
test(set_max_integer_size) :-
    catch(
            foo([9000,2000,10000]),
            error(resource_error(_),_),
            fail
    ).

:- end_tests(max_integer_size).

:- begin_tests(rms).

test(rms_test_p1_empty) :-
    init('./test'),
    compile('examples/ex_p1.pl',db(edge,[node,node]),M1),
    compute(rms,M1,_),
    assertion(edge1(0,0)),
    assertion(edge1(1,0)),
    assertion(edge1(2,0)).

test(rms_test_p2_no_name) :-
    init('./test'),
    compile('examples/ex_p2.pl',db(edge,[node,node]),M1),
    compute(rms,M1,matrix(M2Name,_,_)),
    assertion(call(M2Name,0,14)),
    assertion(call(M2Name,1,12)),
    assertion(call(M2Name,2,8)),
    assertion(call(M2Name,3,0)).

test(rms_test_p2) :-
    init('./test'),
    compile('examples/ex_p2.pl',db(edge,[node,node]),M1),
    compute(rms,M1,matrix(M2Name,_,_),[output_id='connect']),
    assertion(call(M2Name,0,14)),
    assertion(call(M2Name,1,12)),
    assertion(call(M2Name,2,8)),
    assertion(call(M2Name,3,0)).

test(rms_test_p3) :-
    init('./test'),
    compile('examples/ex_p3.pl',db(edge,[node,node]),M1),
    compute(rms,M1,matrix(M2Name,_,_),[output_id='connect']),
    assertion(call(M2Name,0,1022)),
    assertion(call(M2Name,1,1020)),
    assertion(call(M2Name,2,1016)),
    assertion(call(M2Name,3,1008)),
    assertion(call(M2Name,4,992)),
    assertion(call(M2Name,5,960)),
    assertion(call(M2Name,6,896)),
    assertion(call(M2Name,7,768)),
    assertion(call(M2Name,8,512)),
    assertion(call(M2Name,9,0)).

test(rms_test_p4_with_duplicates) :-
    init('./test'),
    compile('examples/ex_p3.pl',db(edge,[node,node]),M1),
    compute(rms,M1,matrix(M2Name,_,_),[output_id='connect']),
    assertion(call(M2Name,0,1022)),
    assertion(call(M2Name,1,1020)),
    assertion(call(M2Name,2,1016)),
    assertion(call(M2Name,3,1008)),
    assertion(call(M2Name,4,992)),
    assertion(call(M2Name,5,960)),
    assertion(call(M2Name,6,896)),
    assertion(call(M2Name,7,768)),
    assertion(call(M2Name,8,512)),
    assertion(call(M2Name,9,0)).

:- end_tests(rms).

:- begin_tests(smp).

test(smp_test_p1_empty) :-
        init('./test'),
        compile('examples/ex_p1.pl',db(edge,[node,node]),M1),
        lm_select([c1],M1,V1),
        compute(smp,[V1,M1],matrix(M2Name,_,_)),
        assertion(call(M2Name,0,0)).

test(smp_test_p2_no_name) :-
        init('./test'),
        compile('examples/ex_p2.pl',db(edge,[node,node]),M1),
        lm_select([c1],M1,V1),
        compute(smp,[V1,M1],matrix(M2Name,_,_)),
        assertion(call(M2Name,0,14)).

test(smp_test_p3) :-
        init('./test'),
        compile('examples/ex_p3.pl',db(edge,[node,node]),M1),
        lm_select([c2],M1,V1,[output_id='query']),
        compute(smp,[V1,M1],matrix(M2Name,_,_)),
        assertion(call(M2Name,0,1020)).

test(smp_test_p3_multiple_constants) :-
        init('./test'),
        compile('examples/ex_p3.pl',db(edge,[node,node]),M1),
        lm_select([c1,c2],M1,V1,[output_id='query']),
        compute(smp,[V1,M1],matrix(M2Name,_,_)),
        assertion(call(M2Name,0,1022)).

test(smp_test_p4_with_duplicates) :-
        init('./test'),
        compile('examples/ex_p3.pl',db(edge,[node,node]),M1),
        lm_select([c3],M1,V1,[output_id='query']),
        compute(smp,[V1,M1],matrix(M2Name,_,_)),
        assertion(call(M2Name,0,1016)).

:- end_tests(smp).