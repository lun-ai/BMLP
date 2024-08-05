:- export px_entailed/1.

%px_entailed('[[X  > 3*Y + 2],[Y > 0]]')
:- import read_atom_to_term/3 from string.
:- import {}/1 from clpr.
:- import entailed/1 from clpr.
/*
:- import dump/3,projecting_assert/1 from dump.
:- import sup/4, sup/2, maximize/1 from clpr.
:- import inf/4, inf/2, minimize/1 from clpr.
:- import member/2 from basics.
:- import bb_inf/3 from clpr.
*/
px_entailed(Atom):-
    read_atom_to_term(Atom,Cterm,_Cvars),
%    writeln(Cterm),writeln(Cvars),
    Cterm = [Constraints,Eterm],
%    writeln(cnstr(Constraints)),
    Eterm = [E],
    set_up_constraints(Constraints),
    entailed(E),
    writeln(entailment_succeeded).

set_up_constraints([]).
set_up_constraints([H|T]):-
    {H},
    set_up_constraints(T).

