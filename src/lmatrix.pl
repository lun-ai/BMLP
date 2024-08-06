%  core modules
% Do not change the order of imports
:- [setarith].
:- [lmarith].
:- [compile].
:- [compute].
:- [utils].

% preliminary module for lmatrix operations on
%   linear and immediately recursive datalog

:- dynamic srcPath/1,lm_status/1,computation_mode/1.

init(Path) :- var(Path),!,
              atom_concat(Path,'/',Path1),
              assertz(srcPath(Path1)),
              assertz(lm_status(initialised)).
init(Path) :- atom_concat(Path,'/',Path1),
              assertz(srcPath(Path1)),
              assertz(lm_status(initialised)).

compile :-  lm_status(initialised),
            srcPath(Path),
            compile(Path),
            retract(lm_status(initialised)),
            assertz(lm_status(compiled)).

compute :-  lm_status(compiled),
            srcPath(Path),
            compute(Path,_Depth),
            retract(lm_status(compiled)).


bmlp(rms,Args,[matrix(P,[Q,Q])],[matrix(P2,[Q,Q])]) :-
    srcPath(Path),
    lm_fixpoint(rms,Path,P,Depth_),
    atom_concat(P,'_rms',P1),
    new_value(Args,output_id,P1,P2).
bmlp(smp,Args,[vector(P1,[Q])],[matrix(P,[Q,Q])],[vector(P3,[Q])]) :-
    srcPath(Path),
    lm_fixpoint(smp,Path,P,Depth_),
    atom_concat(P,'_smp',P2),
    new_value(Args,output_id,P2,P3).

