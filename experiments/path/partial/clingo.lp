path(A,B) :-
    edge(A,B).
path(A,B) :-
    edge(A,C),
    path(C,B).

path_c1(Y) :- path(c1,Y).
#show path_c1/1.