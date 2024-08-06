:- discontiguous compile_constants/4,compile_lmatrix/5,compile_lmatrix_/6.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compiling square matrices
compile(matrix,P,matrix(P,[Q,Q])) :-
    atom_list_concat([BasePath,Q,'_hbn.pl'],HerbPath),
    lm_mkcton_mul([(T,Objs)],HerbPath),
    compile_lmatrix([Q,Q],P,BasePath,HerbPath,_LMatrixPath).

compile_lmatrix([T1,T2],P,BasePath,HerbPath,LMatrixPath) :- !,
    mode(P,[T1,T2]),
    atom_concat(P,'1',P1),
    atomic_list_concat([BasePath,P1],LMatrixPath),
    consult(HerbPath),
    tell(LMatrixPath),
    forall(cton(T1,C1,X),compile_lmatrix_(P,P1,C1,X,T2)),
    told.
compile_lmatrix_(P,P1,C1,X,T) :-
    findall(Y,(Term=..[P,_,_],current_predicate(_,Term),call(P,C1,C2),cton(T,C2,Y)),Ys),
    lm_stob1(Ys,Bs),
    A=..[P1,X,Bs],
    write_fact(A).

compile_lmatrix(_,_,_,_,_).
compile_constants(_,_,_,_).
