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

:- begin_tests(initialisation).

test(init) :-
    init.

test(init_other_folder) :-
    init('./temp').

:- end_tests(initialisation).

% Tests on compiling all examples into matrices
:- begin_tests(compilation).

test(compile_test_p1_total_order) :-
    init,
    compile('./bmlp/tests/ex_p1.pl',db(edge,[node,node],_),M1),
    lm_consult(M1),
    assertion(edge1(0,0)),
    assertion(edge1(1,0)),
    assertion(edge1(2,0)),
    lm_unload(M1).

test(compile_test_p2) :-
    init,
    compile('./bmlp/tests/ex_p2.pl',db(edge,[node,node],_),M1),
    lm_consult(M1),
    assertion(edge1(0,2)),
    assertion(edge1(1,4)),
    assertion(edge1(2,8)),
    assertion(edge1(3,0)),
    lm_unload(M1).

test(compile_test_p3) :-
    init,
    compile('./bmlp/tests/ex_p3.pl',db(edge,[node,node],_),M1),
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
    assertion(edge1(9,0)),
    lm_unload(M1).

test(compile_test_p4_with_duplicates) :-
    init,
    compile('./bmlp/tests/ex_p4.pl',db(edge,[node,node],_),M1),
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
    assertion(edge1(9,0)),
    lm_unload(M1).

test(compile_test_p5_single_relation) :-
    init,
    compile('./bmlp/tests/ex_p6.pl',db(contains,[location,location],_),M1),
    lm_consult(M1),
    assertion(contains1(0,0)),
    assertion(contains1(1,0)),
    assertion(contains1(2,16)),
    assertion(contains1(3,0)),
    assertion(contains1(4,2)),
    assertion(contains1(5,0)),
    assertion(contains1(6,0)),
    lm_unload(M1).

test(compile_test_p5_multiple_relations) :-
    init,
    compile('./bmlp/tests/ex_p6.pl',db(contains,[location,location],_),M1),
    compile('./bmlp/tests/ex_p6.pl',db(adjoins,[location,location],_),M2),
    lm_consult(M1),
    assertion(contains1(0,0)),
    assertion(contains1(1,0)),
    assertion(contains1(2,16)),
    assertion(contains1(3,0)),
    assertion(contains1(4,2)),
    assertion(contains1(5,0)),
    assertion(contains1(6,0)),
    lm_unload(M1),
    lm_consult(M2),
    assertion(adjoins1(0,0)),
    assertion(adjoins1(1,0)),
    assertion(adjoins1(2,8)),
    assertion(adjoins1(3,0)),
    assertion(adjoins1(4,0)),
    assertion(adjoins1(5,0)),
    assertion(adjoins1(6,0)),
    lm_unload(M2).

test(recompilation) :-
    compile('./bmlp/tests/ex_p3.pl',db(edge,[node,node],_),M1),
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
    assertion(edge1(9,0)),
    lm_unload(M1),
    compile('./bmlp/tests/ex_p5.pl',db(edge,[node,node],_),M2),
    lm_consult(M2),
    assertion(edge1(0,0)),
    assertion(edge1(1,4)),
    assertion(edge1(2,8)),
    assertion(edge1(3,16)),
    assertion(edge1(4,40)),
    assertion(edge1(5,64)),
    assertion(edge1(6,16)),
    assertion(edge1(7,0)),
    assertion(edge1(8,0)),
    assertion(edge1(9,0)),
    lm_unload(M2).


:- end_tests(compilation).

:- begin_tests(boolean_operations).

test(addition_self_add) :-
    init,
    compile('./bmlp/tests/ex_p2.pl',db(edge,[node,node],_),M1),
    add((M1,M1),matrix(Ml2,_,_,_)),
    lm_consult(Ml2),
    atomic_list_concat(Ml2,M2Name),
    assertion(call(M2Name,0,2)),
    assertion(call(M2Name,1,4)),
    assertion(call(M2Name,2,8)),
    assertion(call(M2Name,3,0)),
    lm_unload(M1),
    lm_unload(Ml2).
test(addition_different_matrices) :-
    init,
    compile('./bmlp/tests/ex_p3.pl',db(edge,[node,node],_),M1),
    compile('./bmlp/tests/ex_p5.pl',db(edge,[node,node],_),M2,[output_name='new_edges']),
    add((M1,M2),matrix(Ml3,_,_,_)),
    lm_consult(Ml3),
    atomic_list_concat(Ml3,M3Name),
    assertion(call(M3Name,0,2)),
    assertion(call(M3Name,1,4)),
    assertion(call(M3Name,2,8)),
    assertion(call(M3Name,3,16)),
    assertion(call(M3Name,4,40)),
    assertion(call(M3Name,5,64)),
    assertion(call(M3Name,6,400)),
    assertion(call(M3Name,7,256)),
    assertion(call(M3Name,8,512)),
    assertion(call(M3Name,9,0)),
    lm_unload(M1),
    lm_unload(M2),
    lm_unload(Ml3).

test(multiplication_self_mul) :-
    init,
    compile('./bmlp/tests/ex_p2.pl',db(edge,[node,node],_),M1),
    mul((M1,M1),matrix(Ml2,_,_,_)),
    lm_consult(Ml2),
    atomic_list_concat(Ml2,M2Name),
    assertion(call(M2Name,0,4)),
    assertion(call(M2Name,1,8)),
    assertion(call(M2Name,2,0)),
    assertion(call(M2Name,3,0)),
    lm_unload(M1),
    lm_unload(Ml2).
test(multiplication_different_matrices) :-
    init,
    compile('./bmlp/tests/ex_p3.pl',db(edge,[node,node],_),M1),
    compile('./bmlp/tests/ex_p5.pl',db(edge,[node,node],_),M2,[output_name='new_edges']),
    mul((M1,M2),matrix(Ml3,_,_,_)),
    lm_consult(Ml3),
    atomic_list_concat(Ml3,M3Name),
    assertion(call(M3Name,0,4)),
    assertion(call(M3Name,1,8)),
    assertion(call(M3Name,2,16)),
    assertion(call(M3Name,3,40)),
    assertion(call(M3Name,4,64)),
    assertion(call(M3Name,5,16)),
    assertion(call(M3Name,6,0)),
    assertion(call(M3Name,7,0)),
    assertion(call(M3Name,8,0)),
    assertion(call(M3Name,9,0)),
    lm_unload(M1),
    lm_unload(M2),
    lm_unload(Ml3).

test(additionIdentity_add_to_zero_matrix) :-
    init,
    compile('./bmlp/tests/ex_p1.pl',db(edge,[node,node],_),M1),
    addI(M1,matrix(Ml2,_,_,_)),
    lm_consult(Ml2),
    atomic_list_concat(Ml2,M2Name),
    assertion(call(M2Name,0,1)),
    assertion(call(M2Name,1,2)),
    assertion(call(M2Name,2,4)),
    lm_unload(M1),
    lm_unload(Ml2).
test(additionIdentity) :-
    init,
    compile('./bmlp/tests/ex_p2.pl',db(edge,[node,node],_),M1),
    addI(M1,matrix(Ml2,_,_,_)),
    lm_consult(Ml2),
    atomic_list_concat(Ml2,M2Name),
    assertion(call(M2Name,0,3)),
    assertion(call(M2Name,1,6)),
    assertion(call(M2Name,2,12)),
    assertion(call(M2Name,3,8)),
    lm_unload(M1),
    lm_unload(Ml2).

test(transpose_all_zeros) :-
    init,
    compile('./bmlp/tests/ex_p1.pl',db(edge,[node,node],_),M1),
    transpose(M1,matrix(Ml2,_,_,_)),
    lm_consult(Ml2),
    atomic_list_concat(Ml2,M2Name),
    assertion(call(M2Name,0,0)),
    assertion(call(M2Name,1,0)),
    assertion(call(M2Name,2,0)),
    lm_unload(M1),
    lm_unload(Ml2).
test(transpose_identity) :-
    init,
    compile('./bmlp/tests/ex_p1.pl',db(edge,[node,node],_),M1),
    addI(M1,M2),
    transpose(M2,matrix(Ml3,_,_,_)),
    lm_consult(Ml3),
    atomic_list_concat(Ml3,M3Name),
    assertion(call(M3Name,0,1)),
    assertion(call(M3Name,1,2)),
    assertion(call(M3Name,2,4)),
    lm_unload(M1),
    lm_unload(M2),
    lm_unload(Ml3).
test(transpose_backward_paths) :-
    init,
    compile('./bmlp/tests/ex_p2.pl',db(edge,[node,node],_),M1),
    transpose(M1,matrix(Ml2,_,_,_)),
    lm_consult(Ml2),
    atomic_list_concat(Ml2,M2Name),
    assertion(call(M2Name,0,0)),
    assertion(call(M2Name,1,1)),
    assertion(call(M2Name,2,2)),
    assertion(call(M2Name,3,4)),
    lm_unload(M1),
    lm_unload(Ml2).

test(negation_all_zeros) :-
    init,
    compile('./bmlp/tests/ex_p1.pl',db(edge,[node,node],_),M1),
    negate(M1,matrix(Ml2,_,_,_)),
    lm_consult(Ml2),
    atomic_list_concat(Ml2,M2Name),
    assertion(call(M2Name,0,7)),
    assertion(call(M2Name,1,7)),
    assertion(call(M2Name,2,7)),
    lm_unload(M1),
    lm_unload(Ml2).
test(negation_not_reachable) :-
    init,
    compile('./bmlp/tests/ex_p2.pl',db(edge,[node,node],_),M1),
    negate(M1,matrix(Ml2,_,_,_)),
    lm_consult(Ml2),
    atomic_list_concat(Ml2,M2Name),
    assertion(call(M2Name,0,13)),
    assertion(call(M2Name,1,11)),
    assertion(call(M2Name,2,7)),
    assertion(call(M2Name,3,15)),
    lm_unload(M1),
    lm_unload(Ml2).


:- end_tests(boolean_operations).

% Tests on runing BMLP-RMS on example datalog programs
:- begin_tests(rms).

test(rms_test_p1_empty) :-
    init,
    compile('./bmlp/tests/ex_p1.pl',db(edge,[node,node],_),M1),
    rms(M1,_),
    assertion(edge1(0,0)),
    assertion(edge1(1,0)),
    assertion(edge1(2,0)).

test(rms_test_p2_no_name) :-
    init,
    compile('./bmlp/tests/ex_p2.pl',db(edge,[node,node],_),M1),
    rms(M1,matrix(Ml2,_,_,_)),
    atomic_list_concat(Ml2,M2Name),
    assertion(call(M2Name,0,14)),
    assertion(call(M2Name,1,12)),
    assertion(call(M2Name,2,8)),
    assertion(call(M2Name,3,0)).

test(rms_test_p2) :-
    init,
    compile('./bmlp/tests/ex_p2.pl',db(edge,[node,node],_),M1),
    rms(M1,matrix(Ml2,_,_,_),[output_name='path']),
    atomic_list_concat(Ml2,M2Name),
    assertion(call(M2Name,0,14)),
    assertion(call(M2Name,1,12)),
    assertion(call(M2Name,2,8)),
    assertion(call(M2Name,3,0)).

test(rms_test_p3_linear) :-
    init,
    compile('./bmlp/tests/ex_p3.pl',db(edge,[node,node],_),M1),
    rms(M1,matrix(Ml2,_,_,_),[output_name='path']),
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
    init,
    compile('./bmlp/tests/ex_p3.pl',db(edge,[node,node],_),M1),
    rms(M1,matrix(Ml2,_,_,_),[output_name='path']),
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

test(rms_test_p5_with_cycles) :-
    init,
    compile('./bmlp/tests/ex_p5.pl',db(edge,[node,node],_),M1),
    rms(M1,matrix(Ml2,_,_,_),[output_name='path']),
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

test(rms_test_p7_large_program) :-
        init,
        compile('./bmlp/tests/ex_p7.pl',db(edge,[node,node],_),M1),
        rms(M1,matrix(Ml2,_,_,_)),
        atomic_list_concat(Ml2,M2Name),
        assertion(call(M2Name,0,10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069375)).

:- end_tests(rms).


% Tests on runing BMLP-SMP on example datalog programs
:- begin_tests(smp).

test(smp_test_p1_empty) :-
        init,
        compile('./bmlp/tests/ex_p1.pl',db(edge,[node,node],_),M1),
        lm_select([c1],M1,V1),
        smp((V1,M1),matrix(Ml2,_,_,_)),
        atomic_list_concat(Ml2,M2Name),
        assertion(call(M2Name,0,0)).

test(smp_test_p2_no_name) :-
        init,
        compile('./bmlp/tests/ex_p2.pl',db(edge,[node,node],_),M1),
        lm_select([c1],M1,V1),
        smp((V1,M1),matrix(Ml2,_,_,_)),
        atomic_list_concat(Ml2,M2Name),
        assertion(call(M2Name,0,14)).

test(smp_test_p3) :-
        init,
        compile('./bmlp/tests/ex_p3.pl',db(edge,[node,node],_),M1),
        lm_select([c2],M1,V1,[output_name='query']),
        smp((V1,M1),matrix(Ml2,_,_,_)),
        atomic_list_concat(Ml2,M2Name),
        assertion(call(M2Name,0,1020)).

test(smp_test_p3_multiple_constants) :-
        init,
        compile('./bmlp/tests/ex_p3.pl',db(edge,[node,node],_),M1),
        lm_select([c1,c2],M1,V1,[output_name='query']),
        smp((V1,M1),matrix(Ml2,_,_,_)),
        atomic_list_concat(Ml2,M2Name),
        assertion(call(M2Name,0,1022)).

test(smp_test_p4_with_duplicates) :-
        init,
        compile('./bmlp/tests/ex_p3.pl',db(edge,[node,node],_),M1),
        lm_select([c3],M1,V1,[output_name='query']),
        smp((V1,M1),matrix(Ml2,_,_,_)),
        atomic_list_concat(Ml2,M2Name),
        assertion(call(M2Name,0,1016)).

test(smp_test_p5_with_cycles) :-
        init,
        compile('./bmlp/tests/ex_p5.pl',db(edge,[node,node],_),M1),
        lm_select([c2],M1,V1,[output_name='query']),
        smp((V1,M1),matrix(Ml2,_,_,_)),
        atomic_list_concat(Ml2,M2Name),
        assertion(call(M2Name,0,124)).

% A larger program with 1000 constants and 9848 facts
% the binary code represent has been converted from output of a SWI-Prolog program
test(smp_test_p7_large_program) :-
        init,
        compile('./bmlp/tests/ex_p7.pl',db(edge,[node,node],_),M1),
        lm_select([c1],M1,V1,[output_name='query']),
        smp((V1,M1),matrix(Ml2,_,_,_)),
        atomic_list_concat(Ml2,M2Name),
        assertion(call(M2Name,0,10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069375)).

:- end_tests(smp).

:- begin_tests(composed).

test(composed_p6) :-
        init,
        compile('./bmlp/tests/ex_p6.pl',db(contains,[location,location],_),M1),
        compile('./bmlp/tests/ex_p6.pl',db(adjoins,[location,location],_),M2),
        rms(M1,M3),
        compute(transpose,M3,MT3),
        compute(addI,MT3,MIT3),
        compute(transpose,M2,MT2),
        compute(add,[M2,MT2],M4),
        compute(mul,[MIT3,M4],M5),
        compute(negate,M5,matrix(Ml,_,_,_)),
        atomic_list_concat(Ml,MName),
        lm_consult(Ml),
        assertion(call(MName,0,127)),
        assertion(call(MName,1,119)),
        assertion(call(MName,2,119)),
        assertion(call(MName,3,123)),
        assertion(call(MName,4,119)),
        assertion(call(MName,5,127)),
        assertion(call(MName,6,127)).

:- end_tests(composed).

:- begin_tests(matrix_to_facts).
test(lm_to_facts) :-
    init,
    compile('./bmlp/tests/ex_p2.pl',db(edge,[node,node],_),M1),
    lm_consult(M1),
    lm_to_facts(M1,Fs),
    assertion(member(edge(c1,c2),Fs)),
    assertion(member(edge(c2,c3),Fs)),
    assertion(member(edge(c3,c4),Fs)),
    lm_unload(M1).
:- end_tests(matrix_to_facts).