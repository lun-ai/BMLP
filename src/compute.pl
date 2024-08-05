:- discontiguous lm_fixpoint/4, fixedpoint/5, fixedpoint_/5.
:- use_module(library(time)).

compute(BasePath,Depth) :-
    computation_mode(M),
    mode(P,_),
    call_time(lm_fixpoint(M,BasePath,P,Depth),Stats),
    get_dict(cpu,Stats,CpuT),
    writeln(CpuT).
%    profile(lm_fixpoint(M,BasePath,P,Depth),[time(cpu)]).

lm_fixpoint(ime,Path,P,_Depth) :-
    atomic_list_concat([Path,P,'1'],LMatrix),
    consult(LMatrix),
    % compute the transpose of the boolean matrix
    lm_prod_trans(Path,[P,2],_),
    % calculate the fixpoint
    call_time(fixedpoint(ime,Path,P,_SNum),Stats),
    get_dict(cpu,Stats,CpuT),
    writeln(CpuT).
fixedpoint(ime,Path,P,_) :-
    mode(P,[Q,Q]),
    marking(S),!,
    cton(Q,S,X),
    lm_stob1([X],BXs),
    % initialise initial marking as a one-hot vector
    write_row_matrix(Path,P,5,0,BXs),
    fixedpoint_(ime,Path,P,5,_).
fixedpoint(ime,_,_,_) :- !.
fixedpoint_(ime,Path,P,N,SNum) :-
    N2 is N + 2,
    all_submatrix(Path,[P,N],[P,1],[P,N2]),
%    N2 is N1 + 2,
%    lm_subtract(Path,[P,N1],[P,2],[P,N2]),
    lm_prod(Path,[P,N2],[P,2],[P,N3]),
    N4 is N + 10,
    lm_add(Path,[P,N],[P,N3],[P,N4]),
    \+lm_eq([P,N],[P,N4]),!,
    % next iteration
    fixedpoint_(ime,Path,P,N4,SNum).
fixedpoint_(ime,_,_,N,N) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using square matrices (specialised)
lm_fixpoint(smp,Path,P,Depth) :-
    mode(P,[Q,Q]),
    marking(S),!,
    cton(Q,S,X),
    lm_stob1([X],BXs),
    % initialise initial marking as a one-hot vector
    mname(P,'v',V0),
%    M1 @@ Path is_lmatrix_p [P,1] @@ Path \/ 1,        % M = R + I (identify matrix)
%    lm_prod_trans(Path,M1,_),
    write_row_matrix(Path,V0,0,0,BXs),
    atomic_list_concat([Path,P,'1'],SqM),
    consult(SqM),
    fixedpoint(smp,Path,[P,1],[V0,0],Depth).
fixedpoint(smp,Path,[MName,N],[VName,N1],Depth) :-
    mname(VName,'new',VName1),
	[VName1,N1] @@ Path is_lmatrix_p [VName,N1] @@ Path * [MName,N] @@ Path,
	% check for closure
	\+(lm_submatrix([VName1,N1],[VName,N1])),
	N2 is N1 + 1,
	[VName,N2] @@ Path is_lmatrix_p [VName,N1] @@ Path + [VName1,N1] @@ Path,
	fixedpoint(smp,Path,[MName,N],[VName,N2],Depth), !.
fixedpoint(smp,_,_,[_,Depth],Depth) :-	% Reached fixed point
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% using square matrices
lm_fixpoint(rms,Path,P,Depth) :-
    atomic_list_concat([Path,P,'1'],LMatrix),
    consult(LMatrix),
    M1 @@ Path is_lmatrix_p [P,1] @@ Path \/ 1,         % M = R + I (identify matrix)
    fixedpoint(rms,Path,M1,1,Depth).
%	fixedpoint(rms,Path,M1,1,Depth),writes(['Fixed point is ',Depth,'\n']).
fixedpoint(rms,Path,M1,N1,Depth) :-
	M2 @@ Path is_lmatrix_p M1 @@ Path ^2,              % M^2n = M^n x M^n repeated squaring
%	writes(['At depth ', N1, ' ... \n']),
	\+(lm_submatrix(M2,M1)), N2 is N1*2,                % check for closure
	% writes([M1,' is a proper subset of ',M2,'\n']),
	fixedpoint(rms,Path,M2,N2,Depth), !.
fixedpoint(rms,_,_,Depth,Depth) :-	% Reached fixed point
	!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% swi prolog
%:- set_prolog_flag(table_space, 16000000000).
%:- set_prolog_flag(stack_limit, 16000000000).
%lm_fixpoint(swipl,_,_,_) :-
%    target(P),
%    table(P/2),
%    current_predicate(marking/1),!,
%    marking(S),
%    call_time(closure(P,S),Stats),
%    get_dict(cpu,Stats,CpuT),
%    writeln(CpuT).
%lm_fixpoint(swipl,_,_,_) :-
%    target(P),
%    table(P/2),
%    call_time(closure(P),Stats),
%    get_dict(cpu,Stats,CpuT),
%    writeln(CpuT).
%
%closure(P,C1) :- call(P,C1,_C2), fail.
%closure(_,_).
%closure(P) :- call(P,_C1,_C2), fail.
%closure(_).