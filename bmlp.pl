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
        lm_consult/1,
        lm_print/1,
        lm_select/3,
        lm_select/4,
        mul/2,
        mul/3,
        add/2,
        add/3,
        addI/2,
        addI/3,
        transpose/2,
        transpose/3,
        negate/2,
        negate/3,
        rms/2,
        rms/3,
        smp/2,
        smp/3
    ]).

%  core modules
:- ['bmlp/init.pl'].
:- ['bmlp/setarith.pl'].
:- ['bmlp/lmarith.pl'].
:- ['bmlp/compile.pl'].
:- ['bmlp/compute.pl'].
:- ['bmlp/utils.pl'].
:- ['bmlp/test.pl'].



