%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   BMLP modules
%   Author: Lun Ai and S.H. Muggleton
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- discontiguous lm_fixpoint/4,fixedpoint/5,fixedpoint_/5.
:- use_module(library(time)).

compute(A,B,C) :-
    compute(A,B,C,_Args).
compute(rms,matrix(P,[Q,Q],Dim),matrix(P2,[Q,Q],Dim),Args) :-
    srcPath(Path),
    new_value(Args,output_id,P,P1),
    rms(Path,P,P1,Depth),
    mname(P1,Depth,P2).
compute(smp,[matrix(V1,[Q],[D]),matrix(P,[Q,Q],[D,D])],matrix(V2,[Q],[D]),_Args) :-
    srcPath(Path),
    smp(Path,V1,P,Depth),
    mname(V1,Depth,V2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Selective matrix product (SMP)
smp(Path,V,P,Depth) :-
    % initialise input as a one-hot vector
    atomic_list_concat([Path,P,'1'],LMatrix),
    consult(LMatrix),
%    trace,
    mname(V,'temp',V1),
    fixedpoint(smp,Path,[P,1],[V,1],[V1,1],[V,Depth]).
fixedpoint(smp,Path,Ml,Vl_init,[V1,N1],Vl_final) :-
    ((N1==1) ->
        (Vl=Vl_init);
        (Vl=[V1,N1])
    ),Vl_temp=[V1,_],
	fixedpoint_(Path,Ml,Vl,Vl_temp),
	% check for closure
	\+(lm_submatrix(Vl_temp,Vl)),!,
	fixedpoint(smp,Path,Ml,Vl_init,Vl_temp,Vl_final).
fixedpoint(smp,Path,Ml,[V,_],[V1,1],[V,2]) :-
    [V,2] @@ Path is_lmatrix_p [V1,2] @@ Path * Ml @@ Path,
	!.
fixedpoint(smp,Path,Ml,[V,_],Vl_temp,[V,N]) :-	% Reached fixed point
    [V,N] @@ Path is_lmatrix_p Vl_temp @@ Path * Ml @@ Path,
	!.
fixedpoint_(Path,Ml,Vl,Vl_temp) :-
    Vl_temp @@ Path is_lmatrix_p Vl @@ Path * Ml @@ Path,
	Vl_temp @@ Path is_lmatrix_p Vl @@ Path + Vl_temp @@ Path.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Repeated matrix squaring (RMS)
rms(Path,P,P1,Depth) :-
    atomic_list_concat([Path,P,'1'],LMatrix),
    consult(LMatrix),
    % M = R + I (identify matrix)
    mname(P1,'temp',P2),
    [P2,1] @@ Path is_lmatrix_p [P,1] @@ Path \/ 1,
    fixedpoint(rms,Path,[P,1],[P2,1],[P1,Depth]).
fixedpoint(rms,Path,Ml_init,Ml_temp,Ml_final) :-
    % M^2n = M^n x M^n repeated squaring
	Ml_temp1 @@ Path is_lmatrix_p Ml_temp @@ Path ^2,
	% check for closure
	\+(lm_submatrix(Ml_temp1,Ml_temp)),!,
	fixedpoint(rms,Path,Ml_init,Ml_temp1,Ml_final).
fixedpoint(rms,Path,Ml_init,Ml_temp,Ml_final) :-	% Reached fixed point
    Ml_final @@ Path is_lmatrix_p Ml_temp @@ Path * Ml_init @@ Path,
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% implementation for iterative extension method
%   working best for multi-arity datalog

%lm_fixpoint(ime,Path,P,_Depth) :-
%    atomic_list_concat([Path,P,'1'],LMatrix),
%    consult(LMatrix),
%    % compute the transpose of the boolean matrix
%    lm_prod_trans(Path,[P,2],_),
%    % calculate the fixpoint
%    call_time(fixedpoint(ime,Path,P,_SNum),Stats),
%    get_dict(cpu,Stats,CpuT),
%    writeln(CpuT).
%fixedpoint(ime,Path,P,_) :-
%    mode(P,[Q,Q]),
%    marking(S),!,
%    cton(Q,S,X),
%    lm_stob1([X],BXs),
%    % initialise initial marking as a one-hot vector
%    write_row_matrix(Path,P,5,0,BXs),
%    fixedpoint_(ime,Path,P,5,_).
%fixedpoint(ime,_,_,_) :- !.
%fixedpoint_(ime,Path,P,N,SNum) :-
%    N2 is N + 2,
%    all_submatrix(Path,[P,N],[P,1],[P,N2]),
%%    N2 is N1 + 2,
%%    lm_subtract(Path,[P,N1],[P,2],[P,N2]),
%    lm_prod(Path,[P,N2],[P,2],[P,N3]),
%    N4 is N + 10,
%    lm_add(Path,[P,N],[P,N3],[P,N4]),
%    \+lm_eq([P,N],[P,N4]),!,
%    % next iteration
%    fixedpoint_(ime,Path,P,N4,SNum).
%fixedpoint_(ime,_,_,N,N) :- !.