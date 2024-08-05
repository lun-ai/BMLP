:- module(c_arith,
	  [ between/3,			% +int, +int, ?int
	    succ/2			% int <-> int
	  ]).

/*
% XSB succ/2 -- please replace if there is a better one to use.
% TLS: not putting in errors (they are caught by is/2).
succ(First,Second):- 
    (number(First) -> Second is First + 1
    ; (number(Second) -> First is Second - 1
      ; Second is First + 1
      )
    ).
*/

%% Modified from stackoverflow.com/questions/4234001/prolog-definition-of-succ-2
%% Bidirectionall successor; positive int
%% True if N1 = N0 + 1 and N0 >= 0. At least one of the arguments
%% must be instantiated to a natural number.
%% This predicate raises the domain error not_less_than_zero if called with
%% a negative integer.
%% E.g. succ(X, 0) fails silently and succ(X, -1) raises a domain error.
succ(N0, N1) :-
        ( properly_grounded(N0) ->  N1 is N0 + 1
        ; properly_grounded(N1) ->  N1 > 0, N0 is N1 - 1
        ; Ctx=context(succ/2,''),
            throw(error(instantiation_error,Ctx))
        ).
%% positive integer
properly_grounded(X):-
        number(X),
        ( X >= 0 -> true
        ; Ctx = context(succ/2,X),
            E=domain_error(not_less_than_zero,X),
            throw(error(E,Ctx));otherwise
        ).





%%% Local Variables: 
%%% mode: prolog
%%% End: 
