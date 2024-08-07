%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Matrix compilation
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compiling square matrices
compile(DBPath,DB,M) :-
    compile(DBPath,DB,M,_Args).
compile(DBPath,DB,matrix(P,[T,T],Dim),_Args) :-
    consult(DBPath),
    DB=..[db,P,[T,T]],
    srcPath(BasePath),
    compile_constants(DB,BasePath,HerbPath,Dim),
    compile_lmatrix(DB,BasePath,HerbPath,_LMatrixPath),
    unload_file(DBPath).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% map constants to a subset of natural numbers in a file
%   record the dimention as the largest natural number assigned
compile_constants(DB,BasePath,HerbPath,[D,D]) :-
    DB=..[db,P,[T,T]],!,
    setof(C,call(T,C),Cs),
    length(Cs,D),
    atomic_list_concat([BasePath,P,'_hbn.pl'],HerbPath),
    lm_mkcton_mul([(T,Cs)],HerbPath).
% for non-square matrices
%   record the dimention of each domain
compile_constants(DB,BasePath,HerbPath,[Q1,Q2]) :-
    DB=..[db,P,[T1,T2]],!,
    setof(A1,call(T1,A1),Cs1),
    setof(A2,call(T2,A2),Cs2),
    length(Cs1,Q1),
    length(Cs2,Q2),
    atom_list_concat([BasePath,P,'_hbn.pl'],HerbPath),
    lm_mkcton_mul([(T1,Cs1),(T2,Cs2)],HerbPath).
compile_constants(_,_,_,_) :-
    throw(error(bmlp_compilation_error(constants),_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compiling an incident matrice from ground facts and constants
compile_lmatrix(DB,BasePath,HerbPath,LMatrixPath) :-
    DB=..[db,P,[T,T]],!,
    atom_concat(P,'1',P1),
    atomic_list_concat([BasePath,P1],LMatrixPath),
    consult(HerbPath),
    tell(LMatrixPath),
    forall(cton(T,C1,X),compile_lmatrix_(P,P1,C1,X,T)),
    told.
compile_lmatrix(DB,BasePath,HerbPath,LMatrixPath) :-
    DB=..[db,P,[T1,T2]],!,
    atom_concat(P,'1',P1),
    atomic_list_concat([BasePath,P1],LMatrixPath),
    consult(HerbPath),
    tell(LMatrixPath),
    forall(cton(T1,C1,X),compile_lmatrix_(P,P1,C1,X,T2)),
    told.
compile_lmatrix(_,_,_,_) :-
    throw(error(bmlp_compilation_error(matrix),_)).
compile_lmatrix_(P,P1,C1,X,T) :-
    findall(Y,(Term=..[P,_,_],current_predicate(_,Term),call(P,C1,C2),cton(T,C2,Y)),Ys),
    lm_stob1(Ys,Bs),
    A=..[P1,X,Bs],
    write_fact(A).
