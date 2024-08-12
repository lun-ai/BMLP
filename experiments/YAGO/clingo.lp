route(A,B):-neighbors(A,B).
route(A,B):-neighbors(A,C),route(C,B).