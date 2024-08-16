path(A,B) :-
    edge(A,B).
path(A,B) :-
    edge(A,C),
    path(C,B).
#show path/2.