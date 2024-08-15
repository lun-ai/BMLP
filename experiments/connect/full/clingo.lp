connect(A,B) :-
    edge(A,B).
connect(A,B) :-
    edge(A,C),
    connect(C,B).
#show connect/2.