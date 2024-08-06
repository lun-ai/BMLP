:- use_module(library(random)).
:- ['src/utils'].
:- ['src/lmarith'].

:- begin_tests(max_integer_size).

foo(Numbers) :-
	concurrent_forall(
	    member(N,Numbers),
        call(foo_,N)
	).
foo_(N) :-
    lm_stob1([N],_).
test(set_max_integer_size) :-
    catch(
            foo([9000,2000,10000]),
            error(resource_error(_),_),
            fail
    ).

:- end_tests(max_integer_size).

:- begin_tests(rms).

test(rms_without_name) :-
    consult(test_p1),
    compile(matrix(edge,M1),
    bmlp(rms,[],M1,M2),
    consult(M2).
test(rms_correctness1) :-
    consult(test_p2),
    compile(matrix,edge,M1),
    bmlp(rms,[output_id='connect'],M1,M2),
    consult(M2).
test(rms_correctness2) :-
    consult(test_p3),
    compile(matrix,edge,M1),
    bmlp(rms,[output_id='connect'],M1,M2),
    consult(M2).

:- end_tests(rms).

:- begin_tests(smp).

test(smp) :-
    compile(matrix,edge,c1,V1),
    compile(matrix,edge,M1),
    bmlp(smp,[],V1,M1,V2),
    consult(V2).

:- end_tests(smp).