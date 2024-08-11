p_1(X,Y) :- t(X,Y).
p_1(X,Y) :- p_2(X,Z), p_1(Z,Y).
p_2(X,Y) :- g(X,Y).
p_2(X,Y) :- p_1(X,Z), p_2(Z,Y).
q(X,Y) :- p_1(X,Y).
q(X,Y) :- p_2(X,Y).
q(X,Y) :- q(Y,X).
f(X,Y) :- set(X), set(Y), \+ q(X,Y).

 :- table disjoint/2,q/2,p_1/2,p_2/2.

set(g1).
set(g2).
set(g3).
set(g4).
set(t1).
set(t2).
set(t3).

set_g(g1).
set_g(g2).
set_g(g3).
set_g(g4).

set_t(t1).
set_t(t2).
set_t(t3).

t(t1,g2).
t(t3,g1).
g(g2,t3).
