%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   BMLP modules unit tests
%   Author: Lun Ai and S.H. Muggleton
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(random)).
:- use_module(bmlp).

:- begin_tests(max_integer_size).

foo(Numbers) :-
	concurrent_forall(
	    member(N,Numbers),
        call(foo_,N)
	).
foo_(N) :-
    bmlp:lm_stob1([N],_).

% check if length-N bitcodes fit the configured integer size
test(set_max_integer_size) :-
    catch(
            foo([9000,2000,10000]),
            error(resource_error(_),_),
            fail
    ).

:- end_tests(max_integer_size).

:- begin_tests(rms).

test(rms_test_p1_no_name) :-
    init,
    compile('examples/ex_p1.pl',db(edge,[node,node]),M1),
    compute(rms,[M1],[_]),
    assertion(bmlp:edge_rms1(0,1)),
    assertion(bmlp:edge_rms1(1,2)),
    assertion(bmlp:edge_rms1(2,4)).
test(rms_test_p2) :-
    init,
    compile('examples/ex_p2.pl',db(edge,[node,node]),M1),
    compute(rms,[M1],[_],[output_id='connect']),
    assertion(bmlp:connect4(0,15)),
    assertion(bmlp:connect4(1,14)),
    assertion(bmlp:connect4(2,12)),
    assertion(bmlp:connect4(3,8)).
test(rms_test_p3) :-
    init,
    compile('examples/ex_p3.pl',db(edge,[node,node]),M1),
    compute(rms,[M1],[_],[output_id='connect']),
    assertion(bmlp:connect8(0,1023)),
    assertion(bmlp:connect8(1,2)),
    assertion(bmlp:connect8(2,1022)),
    assertion(bmlp:connect8(3,1018)),
    assertion(bmlp:connect8(4,1010)),
    assertion(bmlp:connect8(5,994)),
    assertion(bmlp:connect8(6,962)),
    assertion(bmlp:connect8(7,898)),
    assertion(bmlp:connect8(8,770)),
    assertion(bmlp:connect8(9,514)).

:- end_tests(rms).

:- begin_tests(smp).

test(smp) :-
    compile(matrix,edge,c1,V1),
    compile(matrix,edge,M1),
    bmlp(smp,[],V1,M1,V2),
    consult(V2).

:- end_tests(smp).