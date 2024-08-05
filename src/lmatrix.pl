%  core modules
% Do not change the order of imports
:- [setarith].
:- [lmarith].
:- [compile].
:- [compute].
:- [utils].

% preliminary module for lmatrix operations on
%   linear and immediately recursive datalog

:- dynamic srcPath/1,status/1,computation_mode/1.

init(Path) :- atom_concat(Path,'/',Path1),
              assertz(srcPath(Path1)),
              assertz(status(initialised)).

compile :-  status(initialised),
            srcPath(Path),
            compile(Path),
            retract(status(initialised)),
            assertz(status(compiled)).

compute :-  status(compiled),
            srcPath(Path),
            compute(Path,_Depth),
            retract(status(compiled)).
