%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   BMLP modules
%   Author: Lun Ai and S.H. Muggleton
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- discontiguous lm_fixpoint/4, fixedpoint/5, fixedpoint_/5.
:- use_module(library(time)).

compute(A,B,C) :-
    compute(A,B,C,_Args).
compute(rms,[matrix(P,[Q,Q],Dim)],[matrix(P3,[Q,Q],Dim)],Args) :-
    srcPath(Path),
    atom_concat(P,'_rms',P1),
    new_value(Args,output_id,P1,P2),
    lm_fixpoint(rms,Path,P,P2,Depth),
    atom_concat(P2,Depth,P3).
compute(smp,[vector(V1,[Q]),matrix(P,[Q,Q])],[vector(P3,[Q])],Args) :-
    srcPath(Path),
    lm_fixpoint(smp,Path,P,_Depth),
    atom_concat(P1,'_smp',P2),
    new_value(Args,output_id,P2,P3).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Selective matrix product (SMP)
lm_fixpoint(smp,Path,P,Depth) :-
    mode(P,[Q,Q]),
    marking(S),!,
    cton(Q,S,X),
    lm_stob1([X],BXs),
    % initialise input as a one-hot vector
    mname(P,'v',V0),
    write_row_matrix(Path,V0,0,0,BXs),
    atomic_list_concat([Path,P,'1'],SqM),
    consult(SqM),
    fixedpoint(smp,Path,[P,1],[V0,0],Depth).
fixedpoint(smp,Path,[MName,N],[VName,N1],Depth) :-
    mname(VName,'temp',VName1),
	[VName1,N1] @@ Path is_lmatrix_p [VName,N1] @@ Path * [MName,N] @@ Path,
	% check for closure
	\+(lm_submatrix([VName1,N1],[VName,N1])),
	N2 is N1 + 1,
	[VName,N2] @@ Path is_lmatrix_p [VName,N1] @@ Path + [VName1,N1] @@ Path,
	fixedpoint(smp,Path,[MName,N],[VName,N2],Depth), !.
fixedpoint(smp,_,_,[_,Depth],Depth) :-	% Reached fixed point
	!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Repeated matrix squaring (RMS)
lm_fixpoint(rms,Path,P,P1,Depth) :-
    atomic_list_concat([Path,P,'1'],LMatrix),
    consult(LMatrix),
    % M = R + I (identify matrix)
    [P1,1] @@ Path is_lmatrix_p [P,1] @@ Path \/ 1,
    fixedpoint(rms,Path,[P1,1],1,Depth).
fixedpoint(rms,Path,M1,N1,Depth) :-
    % M^2n = M^n x M^n repeated squaring
	M2 @@ Path is_lmatrix_p M1 @@ Path ^2,
	% check for closure
	\+(lm_submatrix(M2,M1)),!,N2 is N1*2,
	fixedpoint(rms,Path,M2,N2,Depth).
fixedpoint(rms,_,_,Depth,Depth) :-	% Reached fixed point
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