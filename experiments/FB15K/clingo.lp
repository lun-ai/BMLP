p_1(X, Y) :- contains(X, Y).
p_1(X, Y) :- contains(X, Z), p_1(Z, Y).

p_2(X, Y) :- adjoins(X, Y).
p_2(X, Y) :- adjoins(Y, X).
p_2(X, Y) :- p_1(Z, X), p_2(Z, Y).

f(X, Y) :- location(X), location(Y), not p_2(X, Y).