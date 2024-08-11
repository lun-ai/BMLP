%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   BMLP modules
%   Author: Lun Ai and S.H. Muggleton
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- discontiguous lm_fixpoint/4,fixedpoint/5,fixedpoint_/5.
:- use_module(library(time)).

compute(A,B,C) :-
    compute(A,B,C,_Args),!.
compute(_,_,_) :-
    throw(error(bmlp_computation_error,_)).

compute(mul,[matrix([P,N],[Q1,Q2],[D1,D2],_),matrix([P1,N1],[Q2,Q3],[D2,D3],_)],matrix([P2,N2],[Q1,Q3],[D1,D3],_),Args) :-
    srcPath(Path),
    atomic_concat([P,N,'*',P1,N1],P2),
    assign_name(Args,output_id,P2,P3),
    [P3,N2] @@ Path is_lmatrix_p [P,N] @@ Path * [P1,N1] @@ Path,!.

%compute(add,matrix([P,N],[Q,Q],Dim,_),matrix([P1,Depth],[Q,Q],Dim,_),Args) :-
%compute(eq,matrix([P,N],[Q,Q],Dim,_),matrix([P1,Depth],[Q,Q],Dim,_),Args) :-

compute(rms,matrix([P,N],[Q,Q],Dim,_),matrix([P1,Depth],[Q,Q],Dim,_),Args) :-
    srcPath(Path),
    assign_name(Args,output_id,P,P1),
    rms(Path,[P,N],[P1,Depth]),!.
compute(smp,[matrix(V1,[Q],[D],_),matrix(M,[Q,Q],[D,D],_)],matrix(V2,[Q],[D],_),_Args) :-
    srcPath(Path),
    smp(Path,V1,M,V2),!.
compute(_,_,_,_) :-
    throw(error(bmlp_computation_error,_)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Selective matrix product (SMP)
smp(Path,[V,N],[P,N],[V,Depth]) :-
    % initialise input as a one-hot vector
    atomic_list_concat([Path,P,N],LMatrix),
    consult(LMatrix),
    mname(V,'temp',V1),
    fixedpoint(smp,Path,[P,N],[V,N],[V1,N],[V,Depth]).
fixedpoint(smp,Path,Ml,[V,N],[V1,N1],Vl_final) :-
    ((N1==N) ->
        (Vl=[V,N]);
        (Vl=[V1,N1])
    ),Vl_temp=[V1,_],
	fixedpoint_(Path,Ml,Vl,Vl_temp),
	% check for closure
	\+(lm_submatrix(Vl_temp,Vl)),!,
	fixedpoint(smp,Path,Ml,[V,N],Vl_temp,Vl_final).
fixedpoint(smp,Path,Ml,[V,N],[V1,N],[V,N1]) :-
    N1 is N + 1,
    [V,N1] @@ Path is_lmatrix_p [V1,N1] @@ Path * Ml @@ Path,
	!.
fixedpoint(smp,Path,Ml,[V,_],Vl_temp,[V,N]) :-	% Reached fixed point
    [V,N] @@ Path is_lmatrix_p Vl_temp @@ Path * Ml @@ Path,
	!.
fixedpoint_(Path,Ml,Vl,Vl_temp) :-
    Vl_temp @@ Path is_lmatrix_p Vl @@ Path * Ml @@ Path,
	Vl_temp @@ Path is_lmatrix_p Vl @@ Path + Vl_temp @@ Path.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Repeated matrix squaring (RMS)
rms(Path,[P,N],[P1,Depth]) :-
    atomic_list_concat([Path,P,N],LMatrix),
    consult(LMatrix),
    % M = R + I (identify matrix)
    mname(P1,'temp',P2),
    [P2,N] @@ Path is_lmatrix_p [P,N] @@ Path \/ 1,
    fixedpoint(rms,Path,[P,N],[P2,N],[P1,Depth]).
fixedpoint(rms,Path,Ml_init,Ml_temp,Ml_final) :-
    % M^2n = M^n x M^n repeated squaring
	Ml_temp1 @@ Path is_lmatrix_p Ml_temp @@ Path ^2,
	% check for closure
	\+(lm_submatrix(Ml_temp1,Ml_temp)),!,
	fixedpoint(rms,Path,Ml_init,Ml_temp1,Ml_final).
fixedpoint(rms,Path,Ml_init,Ml_temp,Ml_final) :-	% Reached fixed point
    Ml_final @@ Path is_lmatrix_p Ml_temp @@ Path * Ml_init @@ Path,
	!.