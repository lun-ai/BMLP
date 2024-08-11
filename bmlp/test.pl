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

:- begin_tests(compilation).

test(compile_test_p1_total_order) :-
    init('./test'),
    compile('examples/ex_p1.pl',db(edge,[node,node],_),M1),
    lm_consult(M1),
    assertion(edge1(0,0)),
    assertion(edge1(1,0)),
    assertion(edge1(2,0)).

test(compile_test_p2) :-
    init('./test'),
    compile('examples/ex_p2.pl',db(edge,[node,node],_),M1),
    lm_consult(M1),
    assertion(edge1(0,2)),
    assertion(edge1(1,4)),
    assertion(edge1(2,8)),
    assertion(edge1(3,0)).

test(compile_test_p3) :-
    init('./test'),
    compile('examples/ex_p3.pl',db(edge,[node,node],_),M1),
    lm_consult(M1),
    assertion(edge1(0,2)),
    assertion(edge1(1,4)),
    assertion(edge1(2,8)),
    assertion(edge1(3,16)),
    assertion(edge1(4,32)),
    assertion(edge1(5,64)),
    assertion(edge1(6,384)),
    assertion(edge1(7,256)),
    assertion(edge1(8,512)),
    assertion(edge1(9,0)).

test(compile_test_p4_with_duplicates) :-
    init('./test'),
    compile('examples/ex_p4.pl',db(edge,[node,node],_),M1),
    lm_consult(M1),
    assertion(edge1(0,2)),
    assertion(edge1(1,4)),
    assertion(edge1(2,8)),
    assertion(edge1(3,16)),
    assertion(edge1(4,32)),
    assertion(edge1(5,64)),
    assertion(edge1(6,384)),
    assertion(edge1(7,256)),
    assertion(edge1(8,512)),
    assertion(edge1(9,0)).

test(compile_test_p5_single_type) :-
    init('./test'),
    compile('examples/ex_p5.pl',db(red_to_red,[red,red],_),M1),
    lm_consult(M1),
    assertion(red_to_red1(0,0)),
    assertion(red_to_red1(1,0)),
    assertion(red_to_red1(2,0)).

test(compile_test_p5_multiple_types) :-
    init('./test'),
    compile('examples/ex_p5.pl',db(red_to_blue,[red,blue],_),M1),
    lm_consult(M1),
    assertion(red_to_blue1(0,0)),
    assertion(red_to_blue1(1,0)),
    assertion(red_to_blue1(2,0)).

:- end_tests(compilation).

:- begin_tests(rms).

test(rms_test_p1_empty) :-
    init('./test'),
    compile('examples/ex_p1.pl',db(edge,[node,node],_),M1),
    compute(rms,M1,_),
    assertion(edge1(0,0)),
    assertion(edge1(1,0)),
    assertion(edge1(2,0)).

test(rms_test_p2_no_name) :-
    init('./test'),
    compile('examples/ex_p2.pl',db(edge,[node,node],_),M1),
    compute(rms,M1,matrix(Ml2,_,_,_)),
    atomic_list_concat(Ml2,M2Name),
    assertion(call(M2Name,0,14)),
    assertion(call(M2Name,1,12)),
    assertion(call(M2Name,2,8)),
    assertion(call(M2Name,3,0)).

test(rms_test_p2) :-
    init('./test'),
    compile('examples/ex_p2.pl',db(edge,[node,node],_),M1),
    compute(rms,M1,matrix(Ml2,_,_,_),[output_id='connect']),
    atomic_list_concat(Ml2,M2Name),
    assertion(call(M2Name,0,14)),
    assertion(call(M2Name,1,12)),
    assertion(call(M2Name,2,8)),
    assertion(call(M2Name,3,0)).

test(rms_test_p3_linear) :-
    init('./test'),
    compile('examples/ex_p3.pl',db(edge,[node,node],_),M1),
    compute(rms,M1,matrix(Ml2,_,_,_),[output_id='connect']),
    atomic_list_concat(Ml2,M2Name),
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
    compile('examples/ex_p3.pl',db(edge,[node,node],_),M1),
    compute(rms,M1,matrix(Ml2,_,_,_),[output_id='connect']),
    atomic_list_concat(Ml2,M2Name),
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
test(rms_test_p5_with_cyclees) :-
    init('./test'),
    compile('examples/ex_p5.pl',db(edge,[node,node],_),M1),
    compute(rms,M1,matrix(Ml2,_,_,_),[output_id='connect']),
    atomic_list_concat(Ml2,M2Name),
    assertion(call(M2Name,0,0)),
    assertion(call(M2Name,1,124)),
    assertion(call(M2Name,2,120)),
    assertion(call(M2Name,3,120)),
    assertion(call(M2Name,4,120)),
    assertion(call(M2Name,5,120)),
    assertion(call(M2Name,6,120)),
    assertion(call(M2Name,7,0)),
    assertion(call(M2Name,8,0)),
    assertion(call(M2Name,9,0)).

:- end_tests(rms).

:- begin_tests(smp).

test(smp_test_p1_empty) :-
        init('./test'),
        compile('examples/ex_p1.pl',db(edge,[node,node],_),M1),
        lm_select([c1],M1,V1),
        compute(smp,[V1,M1],matrix(Ml2,_,_,_)),
        atomic_list_concat(Ml2,M2Name),
        assertion(call(M2Name,0,0)).

test(smp_test_p2_no_name) :-
        init('./test'),
        compile('examples/ex_p2.pl',db(edge,[node,node],_),M1),
        lm_select([c1],M1,V1),
        compute(smp,[V1,M1],matrix(Ml2,_,_,_)),
        atomic_list_concat(Ml2,M2Name),
        assertion(call(M2Name,0,14)).

test(smp_test_p3) :-
        init('./test'),
        compile('examples/ex_p3.pl',db(edge,[node,node],_),M1),
        lm_select([c2],M1,V1,[output_id='query']),
        compute(smp,[V1,M1],matrix(Ml2,_,_,_)),
        atomic_list_concat(Ml2,M2Name),
        assertion(call(M2Name,0,1020)).

test(smp_test_p3_multiple_constants) :-
        init('./test'),
        compile('examples/ex_p3.pl',db(edge,[node,node],_),M1),
        lm_select([c1,c2],M1,V1,[output_id='query']),
        compute(smp,[V1,M1],matrix(Ml2,_,_,_)),
        atomic_list_concat(Ml2,M2Name),
        assertion(call(M2Name,0,1022)).

test(smp_test_p4_with_duplicates) :-
        init('./test'),
        compile('examples/ex_p3.pl',db(edge,[node,node],_),M1),
        lm_select([c3],M1,V1,[output_id='query']),
        compute(smp,[V1,M1],matrix(Ml2,_,_,_)),
        atomic_list_concat(Ml2,M2Name),
        assertion(call(M2Name,0,1016)).

:- end_tests(smp).