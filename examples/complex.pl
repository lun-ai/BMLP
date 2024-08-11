p_1(X,Y) :- t(X,Y).
p_1(X,Y) :- p_2(X,Z), p_1(Z,Y).
p_2(X,Y) :- g(X,Y).
p_2(X,Y) :- p_1(X,Z), p_2(Z,Y).
q(X,Y) :- p_1(X,Y).
q(X,Y) :- p_2(X,Y).
q(X,Y) :- q(Y,X).
disjoint(X,Y) :- node(X), node(Y), \+ q(X,Y).

 :- table disjoint/2,q/2,p_1/2,p_2/2.

set(b1).
set(b2).
set(b3).
set(b4).
set(r1).
set(r2).
set(r3).

set_g(g1).
set_g(g2).
set_g(g3).
set_g(g4).

set_t(t1).
set_t(t2).
set_t(t3).

t(r1,b2).
t(r3,b1).
g(b2,r3).
