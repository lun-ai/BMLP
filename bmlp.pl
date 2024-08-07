%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%   Modules for BMLP operations
%       on linear recursive datalog
%   Author: Lun Ai and S.H. Muggleton
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(bmlp,
    [
        init/0,
        init/1,
        compile/3,
        compile/4,
        compute/3,
        compute/4,
        lm_consult/1,
        lm_print/1
    ]).

%  core modules
:- ['bmlp/init.pl'].
:- ['bmlp/setarith.pl'].
:- ['bmlp/lmarith.pl'].
:- ['bmlp/compile.pl'].
:- ['bmlp/compute.pl'].
:- ['bmlp/utils.pl'].



