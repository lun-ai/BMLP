%p_1(X,Y) :- contains(X,Y).
%p_1(X,Y) :- contains(X,Z), p_1(Z,Y).
%p_2(X,Y) :- adjoins(X,Y).
%p_2(X,Y) :- adjoins(Y,X).
%p_2(X,Y) :- p_1(Z,X), p_2(Z,Y).
%f(X,Y) :- location(X), location(Y), \+ p_2(X,Y).

location(g1).
location(g2).
location(g3).
location(g4).
location(t1).
location(t2).
location(t3).

contains(t1,g2).
contains(g3,t1).
adjoins(g3,g4).
