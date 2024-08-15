%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   BMLP modules
%   Author: Lun Ai and S.H. Muggleton
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- discontiguous lm_fixpoint/4,fixedpoint/6,fixedpoint_/5.
:- use_module(library(time)).

mul((A,B),C) :-
    compute(mul,[A,B],C).
mul((A,B),C,Arg) :-
    compute(mul,[A,B],C,Arg).
add((A,B),C) :-
    compute(add,[A,B],C).
add((A,B),C,Arg) :-
    compute(add,[A,B],C,Arg).
addI(A,B) :-
    compute(addI,A,B).
addI(A,B,Arg) :-
    compute(addI,A,B,Arg).
transpose(A,B) :-
    compute(transpose,A,B).
transpose(A,B,Arg) :-
    compute(transpose,A,B,Arg).
negate(A,B) :-
    compute(negate,A,B).
negate(A,B,Arg) :-
    compute(negate,A,B,Arg).
rms(A,B) :-
    compute(rms,A,B).
rms(A,B,Arg) :-
    compute(rms,A,B,Arg).
smp((A,B),C) :-
    compute(smp,[A,B],C).
smp((A,B),C,Arg) :-
    compute(smp,[A,B],C,Arg).

compute(A,B,C) :-
    compute(A,B,C,_Args),!.
compute(_,_,_) :-
    throw(error(bmlp_computation_error,_)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Boolean matrix operations

compute(mul,[matrix([P,N],[Q1,Q2],[D1,D2],_),matrix([P1,N1],[Q2,Q3],[D2,D3],_)],
             matrix([P3,N2],[Q1,Q3],[D1,D3],_),Args) :-
    srcPath(Path),
    lm_consult([P,N]),
    lm_consult([P1,N1]),
    atomic_list_concat([P,N,'_mul_',P1,N1,'_'],P2),
    assign_name(Args,output_id,P2,P3),
    [P3,N2] @@ Path is_lmatrix_p ([P,N],[D1,D2]) @@ Path * ([P1,N1],[Q2,Q3]) @@ Path,!.

compute(add,[matrix([P,N],Qs,Dims,_),matrix([P1,N1],Qs,Dims,_)],
             matrix([P3,N2],Qs,Dims,_),Args) :-
    srcPath(Path),
    lm_consult([P,N]),
    lm_consult([P1,N1]),
    atomic_list_concat([P,N,'_add_',P1,N1,'_'],P2),
    assign_name(Args,output_id,P2,P3),
    [P3,N2] @@ Path is_lmatrix_p ([P,N],Dims) @@ Path + ([P1,N1],Dims) @@ Path,!.

compute(addI,matrix(M1,Qs,Dims,_),matrix(M2,Qs,Dims,_),_Args) :-
    srcPath(Path),
    lm_consult(M1),
    M2 @@ Path is_lmatrix_p (M1,Dims) @@ Path \/ 1,!.

compute(transpose,matrix(M1,[Q1,Q2],[D1,D2],_),matrix(M2,[Q2,Q1],[D2,D1],_),_Args) :-
    srcPath(Path),
    lm_consult(M1),
    M2 @@ Path is_lmatrix_p (M1,[D1,D2]) @@ Path ^t,!.

compute(negate,matrix(M1,Qs,Dims,_),matrix(M2,Qs,Dims,_),_Args) :-
    srcPath(Path),
    lm_consult(M1),
    M2 @@ Path is_lmatrix_p \+((M1,Dims)) @@ Path,!.

%compute(eq,matrix([P,N],[Q,Q],Dim,_),matrix([P1,Depth],[Q,Q],Dim,_),Args) :-

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BMLP modules

compute(rms,matrix([P,N],[Q,Q],Dim,_),matrix([P1,Depth],[Q,Q],Dim,_),Args) :-
    srcPath(Path),
    assign_name(Args,output_id,P,P1),
    rms_(Path,[P,N],Dim,[P1,Depth]),!.
compute(smp,[matrix(V1,[Q1,Q],[1,D],_),matrix(M,[Q,Q],[D,D],_)],matrix(V2,[Q1,Q],[1,D],_),_Args) :-
    srcPath(Path),
    smp_(Path,V1,M,[1,D],V2),!.
compute(_,_,_,_) :-
    throw(error(bmlp_computation_error,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Selective matrix product (SMP)
smp_(Path,[V,N],[P,N],Ds,[V,Depth]) :-
    % load matrix
    mfname(Path,P,N,LMatrix),
    consult(LMatrix),
    % load input vector
    mfname(Path,V,N,VName),
    consult(VName),
    mname(V,N,V_N),
    call(V_N,0,X),
    mname(V,'temp',V1),
    mfname(Path,V1,N,V1Name),
    mname(V1,N,V1_N),
    % copy to temp vector
    write_row_matrix_(V1Name,V1_N,0,X,write),
    consult(V1Name),
    fixedpoint(smp,Path,[P,N],[V1,N],Ds,[V,Depth]).
fixedpoint(smp,Path,Ml,[V,N],Ds,Vl_final) :-
	fixedpoint_(Path,Ml,[V,N],[V,N1],Ds),
	% check for closure
    mfname(Path,V,N1,VName),
    consult(VName),
    \+(([V,N1],Ds) @@ Path is_lmatrix_le ([V,N],Ds) @@ Path),!,
	fixedpoint(smp,Path,Ml,[V,N1],Ds,Vl_final).
fixedpoint(smp,Path,Ml,Vl_temp,Ds,Vl_final) :-	% Reached fixed point
    Vl_final @@ Path is_lmatrix_p (Vl_temp,Ds) @@ Path * (Ml,Ds) @@ Path,
	!.
fixedpoint_(Path,Ml,Vl,Vl_temp,Ds) :-
    Vl_temp @@ Path is_lmatrix_p (Vl,Ds) @@ Path * (Ml,Ds) @@ Path,
	Vl_temp @@ Path is_lmatrix_p (Vl,Ds) @@ Path + (Vl_temp,Ds) @@ Path.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Repeated matrix squaring (RMS)
rms_(Path,[P,N],Ds,[P1,Depth]) :-
    atomic_list_concat([Path,P,N],LMatrix),
    consult(LMatrix),
    % M = R + I (identify matrix)
    mname(P1,'temp',P2),
    [P2,N] @@ Path is_lmatrix_p ([P,N],Ds) @@ Path \/ 1,
    fixedpoint(rms,Path,[P,N],[P2,N],Ds,[P1,Depth]).
fixedpoint(rms,Path,Ml_init,Ml_temp,Ds,Ml_final) :-
    % M^2n = M^n x M^n repeated squaring
	Ml_temp1 @@ Path is_lmatrix_p (Ml_temp,Ds) @@ Path ^2,
	% check for closure
	\+((Ml_temp1,Ds) @@ Path is_lmatrix_le (Ml_temp,Ds) @@ Path),!,
	fixedpoint(rms,Path,Ml_init,Ml_temp1,Ds,Ml_final).
fixedpoint(rms,Path,Ml_init,Ml_temp,Ds,Ml_final) :-	% Reached fixed point
    Ml_final @@ Path is_lmatrix_p (Ml_temp,Ds) @@ Path * (Ml_init,Ds) @@ Path,
	!.