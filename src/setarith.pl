%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Set arithmetic
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- [utils].

:- op(700,xfy,is_set). 		% Set expression evaluation
:- op(500,yfx,'--').		% Set minus intersection

% :- compile(library(math)).
S is_set A \/ B :- !,
	Sa is_set A,
	Sb is_set B,
	set_uni(Sa,Sb,S), !.
S is_set A /\ B :- !,
	Sa is_set A,
	Sb is_set B,
	set_int(Sa,Sb,S), !.
S is_set A -- B :- !,
	Sa is_set A,
 	Sb is_set B,
	set_sub(Sa,Sb,S), !.
S is_set A - B :- !,
	S is_set A -- (A /\ B), !.
S is_set A :- !,
	sort(A,S), !.
