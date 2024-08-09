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
    DB=..[db,P,[T,T]],
    consult(DBPath),
    srcPath(BasePath),
    compile_constants(DB,BasePath,CsPath,Dim),
    compile_lmatrix(DB,BasePath,CsPath,_LMatrixPath),
    unload_file(DBPath).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% creating a single-row matrix by selecting
% a subset of the
lm_select(Cs,M,V) :-
    lm_select(Cs,M,V,_Args).
lm_select(Cs,matrix(P,[T,T],[D,D]),matrix(V1,[T],[D]),Args) :-
    srcPath(BasePath),
    atomic_list_concat([BasePath,P,'_csmap.pl'],CsPath),
    consult(CsPath),
    lm_stob(Cs,BXs),
    mname(P,'_q',V),
    new_value(Args,output_id,V,V1),
    write_row_matrix(BasePath,V1,1,0,BXs),!.
lm_select(_,_,_,_) :-
    throw(error(bmlp_select_error,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% map constants to a subset of natural numbers in a file
%   record the dimension as the largest natural number assigned
compile_constants(DB,BasePath,CsPath,[D,D]) :-
    DB=..[db,P,[T,T]],!,
    bagof(C,call(T,C),Cs),
    predsort(term_numerical_order,Cs,Cs1),
    length(Cs1,D),
    atomic_list_concat([BasePath,P,'_csmap.pl'],CsPath),
    lm_mkcton_mul([(T,Cs1)],CsPath).
% for non-square matrices
%   record the dimension of each domain
compile_constants(DB,BasePath,CsPath,[Q1,Q2]) :-
    DB=..[db,P,[T1,T2]],!,
    bagof(A1,call(T1,A1),Cs1),
    bagof(A2,call(T2,A2),Cs2),
    predsort(term_numerical_order,Cs1,Cs3),
    predsort(term_numerical_order,Cs2,Cs4),
    length(Cs3,Q1),
    length(Cs4,Q2),
    atom_list_concat([BasePath,P,'_hbn.pl'],CsPath),
    lm_mkcton_mul([(T1,Cs3),(T2,Cs4)],CsPath).
compile_constants(_,_,_,_) :-
    throw(error(bmlp_compilation_error(constants),_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compiling an incident matrice from ground facts and constants
compile_lmatrix(DB,BasePath,CsPath,LMatrixPath) :-
    DB=..[db,P,[T,T]],!,
    mname(P,'1',P1),
    atomic_list_concat([BasePath,P1],LMatrixPath),
    consult(CsPath),
    tell(LMatrixPath),
    forall(cton(T,C1,X),compile_lmatrix_(P,P1,C1,X,T)),
    told.
compile_lmatrix(DB,BasePath,CsPath,LMatrixPath) :-
    DB=..[db,P,[T1,T2]],!,
    mname(P,'1',P1),
    atomic_list_concat([BasePath,P1],LMatrixPath),
    consult(CsPath),
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