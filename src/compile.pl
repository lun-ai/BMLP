:- discontiguous compile_constants/4,compile_lmatrix/5,compile_lmatrix_/6.

compile(BasePath) :-
    computation_mode(M),
    mode(P,_),
    compile_constants(M,P,BasePath,HerbPath),
    compile_lmatrix(M,P,BasePath,HerbPath,_LMatrixPath).

%% create a mapping from constants to non-negative integers and back
compile_constants(ime,P,BasePath,HerbPath) :- !,
    mode(P,[T,T]),
    nonvar(T),
    findall(Obj,call(T,Obj),Objs),
    atom_concat(BasePath,'herbn.pl',HerbPath),
    lm_mkcton_mul([(T,Objs)],HerbPath).
compile_constants(ime,P,BasePath,HerbPath) :- !,
    mode(P,[T1,T2]),
    nonvar(T1),
    nonvar(T2),
    T1 \== T2,
    findall(Obj,call(T1,Obj),Objs1),
    findall(Obj,call(T2,Obj),Objs2),
    atom_concat(BasePath,'herbn.pl',HerbPath),
    lm_mkcton_mul([(T1,Objs1),(T2,Objs2)],HerbPath).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using square matrices (specialised)
compile_constants(smp,P,BasePath,HerbPath) :- !,
    mode(P,[T,T]),
    nonvar(T),
    findall(Obj,call(T,Obj),Objs),
    atom_concat(BasePath,'herbn.pl',HerbPath),
    lm_mkcton_mul([(T,Objs)],HerbPath).
compile_lmatrix(smp,P,BasePath,HerbPath,LMatrixPath) :- !,
    mode(P,[T1,T2]),
    atom_concat(P,'1',P1),
    atomic_list_concat([BasePath,P1],LMatrixPath),
    consult(HerbPath),
    tell(LMatrixPath),
    forall(cton(T1,C1,X),compile_lmatrix_(smp,P,P1,C1,X,T2)),
    told.
compile_lmatrix_(smp,P,P1,C1,X,T) :-
    findall(Y,(Term=..[P,_,_],current_predicate(_,Term),call(P,C1,C2),cton(T,C2,Y)),Ys),
    lm_stob1(Ys,Bs),
    A=..[P1,X,Bs],
    write_fact(A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using square matrices
compile_constants(rms,P,BasePath,HerbPath) :- !,
    mode(P,[T,T]),
    nonvar(T),
    findall(Obj,call(T,Obj),Objs),
    atom_concat(BasePath,'herbn.pl',HerbPath),
    lm_mkcton_mul([(T,Objs)],HerbPath).
compile_lmatrix(rms,P,BasePath,HerbPath,LMatrixPath) :- !,
    mode(P,[T1,T2]),
    atom_concat(P,'1',P1),
    atomic_list_concat([BasePath,P1],LMatrixPath),
    consult(HerbPath),
    tell(LMatrixPath),
    forall(cton(T1,C1,X),compile_lmatrix_(rms,P,P1,C1,X,T2)),
    told.
compile_lmatrix_(rms,P,P1,C1,X,T) :-
    findall(Y,(Term=..[P,_,_],current_predicate(_,Term),call(P,C1,C2),cton(T,C2,Y)),Ys),
    lm_stob1(Ys,Bs),
    A=..[P1,X,Bs],
    write_fact(A).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create two |T| x |P| boolean matrices T1 and T2
%   where T the number of ground facts and P is the set of constants.
%   (T1)[i,j] = 1 and (T2)[i,k] = 1 if and only if ri(cj,ck) holds.
%
%compile_lmatrix(ime,P,BasePath,HerbPath,LMatrixPath) :- !,
%    mode(P,[T1,T2]),
%    atom_concat(P,'1',P1),
%    atom_concat(P,'2',P3),
%    atomic_list_concat([BasePath,P1],LMatrixPath),
%    consult(HerbPath),
%    tell(LMatrixPath),
%    writes([':- discontiguous(',P1,'/2), discontiguous(',P3,'/2).','\n','\n']),
%    findall(Term,(PredTerm=..[P,_,_],current_predicate(_,PredTerm),call(P,C1,C2),Term=..[P,C1,C2]),Terms),
%    compile_lmatrix_(ime,0,Terms,T1,T2,P1,P3),
%    told.
%compile_lmatrix_(ime,N,[T|Terms],T1,T2,P1,P3) :- !,
%    T=..[_,C1,C2],
%    findall(X,cton(T1,C1,X),Xs),
%    findall(Y,cton(T2,C2,Y),Ys),
%    lm_stob1(Xs,BXs),
%    lm_stob1(Ys,BYs),
%    A=..[P1,N,BXs],
%    B=..[P3,N,BYs],
%    write_fact(A),
%    write_fact(B),
%    N1 is N + 1,
%    compile_lmatrix_(ime,N1,Terms,T1,T2,P1,P3).
%% create emptry matrices when there are no ground facts
%compile_lmatrix_(ime,0,[],_,_,P1,P3) :- !,
%    A=..[P1,0,0],
%    B=..[P3,0,0],
%    write_fact(A),
%    write_fact(B).
%compile_lmatrix_(ime,_,[],_,_,_,_).


compile_lmatrix(_,_,_,_,_).
compile_constants(_,_,_,_).
