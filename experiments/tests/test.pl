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

:- end_tests(rms).

:- begin_tests(smp).

:- end_tests(smp).