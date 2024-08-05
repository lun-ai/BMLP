connect(A,B) :-
    edge(A,B).
connect(A,B) :-
    edge(A,C),
    connect(C,B).

:- not connect(c1,_).