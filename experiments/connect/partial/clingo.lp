connect(A,B) :-
    edge(A,B).
connect(A,B) :-
    edge(A,C),
    connect(C,B).

connect_c1(Y) :- connect(c1,Y).
#show connect_c1/1.