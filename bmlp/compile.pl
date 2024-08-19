%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Matrix compilation
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- multifile(bmlp:ntoc/3),multifile(bmlp:cton/3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compiling matrices
compile(DBPath,DB,M) :-
    compile(DBPath,DB,M,_Args).
% checks if a matrix has been compiled before
compile(DBPath,DB,matrix([P1,1],[T,T],Dim,_),Args) :-
    DB=..[db,P,[T,T],_],
    assign_name(Args,output_name,P,P1),
    \+lm_loaded([P1,1]),!,
    consult(DBPath),
    srcPath(BasePath),
    compile_constants(DB,BasePath,CsPath,Dim),
    compile_lmatrix(DB,BasePath,CsPath,_LMatrixPath,Args),
    unload_file(DBPath).
% if a matrix has been compiled already under the same matrix name
% then recompile it. This creates an updated matrix if the DB has changed.
compile(DBPath,DB,matrix([P1,1],[T,T],Dim,_),Args) :-
    DB=..[db,P,[T,T],_],
    assign_name(Args,output_name,P,P1),
    lm_unload([P1,1]),!,
    consult(DBPath),
    srcPath(BasePath),
    compile_constants(DB,BasePath,CsPath,Dim),
    compile_lmatrix(DB,BasePath,CsPath,_LMatrixPath,Args),
    unload_file(DBPath).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% creating a single-row matrix and select
% a subset of drivable facts
lm_select(Cs,M,V) :-
    lm_select(Cs,M,V,_Args).
lm_select(Cs,matrix([P,_],[T,T],[D,D],_),matrix([V1,1],[T,T],[1,D],_),Args) :-
    srcPath(BasePath),
    atomic_list_concat([BasePath,T,'_csmap.pl'],CsPath),
    consult(CsPath),
    lm_stob(Cs,BXs),
    mname(P,'_q',V),
    assign_name(Args,output_name,V,V1),
    write_row_matrix(BasePath,V1,1,0,BXs),!.
lm_select(_,_,_,_) :-
    throw(error(bmlp_select_error,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% map constants to a subset of natural numbers in a file
%   record the dimension as the largest natural number assigned
compile_constants(DB,BasePath,CsPath,[D,D]) :-
    DB=..[db,_,[T,T],_],!,
    bagof(C,call(T,C),Cs),
    predsort(term_numerical_order,Cs,Cs1),
    length(Cs1,D),
    atomic_list_concat([BasePath,T,'_csmap.pl'],CsPath),
    lm_mkcton_mul([(T,Cs1)],CsPath).
% for non-square matrices
%   record the dimension of each domain
compile_constants(DB,BasePath,CsPath,[Q1,Q2]) :-
    DB=..[db,_,[T1,T2],_],!,
    bagof(A1,call(T1,A1),Cs1),
    bagof(A2,call(T2,A2),Cs2),
    predsort(term_numerical_order,Cs1,Cs3),
    predsort(term_numerical_order,Cs2,Cs4),
    length(Cs3,Q1),
    length(Cs4,Q2),
    atom_list_concat([BasePath,T1,'_',T2,'_csmap.pl'],CsPath),
    lm_mkcton_mul([(T1,Cs3),(T2,Cs4)],CsPath).
compile_constants(_,_,_,_) :-
    throw(error(bmlp_compilation_error(constants),_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% compiling an incident matrice from ground facts and constants
compile_lmatrix(DB,BasePath,CsPath,LMatrixPath,Args) :-
    DB=..[db,P,[T,T],_],!,
    assign_name(Args,output_name,P,P2),
    mname(P2,'1',P1),
    atomic_list_concat([BasePath,P1],LMatrixPath),
    consult(CsPath),
    tell(LMatrixPath),
    forall(cton(T,C1,X),compile_lmatrix_(P,P1,C1,X,T)),
    told.
compile_lmatrix(DB,BasePath,CsPath,LMatrixPath,Args) :-
    DB=..[db,P,[T1,T2],_],!,
    assign_name(Args,output_name,P,P2),
    mname(P2,'1',P1),
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
    write_row_matrix__(P1,X,Bs).
