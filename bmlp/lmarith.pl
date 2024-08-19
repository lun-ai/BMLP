%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Logical Matrix arithmetic package
%	Author: Lun Ai and S.H. Muggleton
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module(library(thread)).
% SWI-Pl 9.2 requires setting max_integer byte size

:- op(700,xfy,is_lmatrix). 		% LMatrix expression evaluation
:- op(700,xfy,is_lmatrix_le). 		% LMatrix expression evaluation
:- op(700,xfy,is_lmatrix_p).    % LMatrix multi-threading extensions (see below)
:- op(500,yfx,'--').			% LMatrix minus intersection
:- op(300,xfy,'@@').

M is_lmatrix A^2 :- !,
	Ma is_lmatrix A,
	lm_square(Ma,M), !.
M is_lmatrix A \/ 1 :- !,
	Ma is_lmatrix A,
	lm_add_diagonal(Ma,M), !.
M is_lmatrix A * B :- !,
	Ma is_lmatrix A,
	Mb is_lmatrix B,
	lm_prod(Ma,Mb,M), !.
M is_lmatrix M :- !, nonvar(M).


% "_p" provides methods for use-defined path to store
%   computed results

% squring
M @@ Path is_lmatrix_p (A,[D1,_]) @@ Path ^2 :- !,
	Ma is_lmatrix A,
	lm_square(Path,Ma,D1,M), !.

% adding identity
M @@ Path is_lmatrix_p (A,[D1,_]) @@ Path \/ 1  :- !,
	Ma is_lmatrix A,
	lm_add_diagonal(Path,Ma,D1,M), !.

% multiplication
M @@ Path is_lmatrix_p (A,[D1,_]) @@ Path * (B,_) @@ Path :- !,
	Ma is_lmatrix A,
	Mb is_lmatrix B,
	lm_prod(Path,Ma,Mb,D1,M), !.

% addition
M @@ Path is_lmatrix_p (A,[D1,D2]) @@ Path + (B,[D1,D2]) @@ Path :- !,
	Ma is_lmatrix A,
	Mb is_lmatrix B,
	lm_add(Path,Ma,Mb,D1,M), !.

% transpose
M @@ Path is_lmatrix_p (A,[D1,D2]) @@ Path ^t:- !,
	Ma is_lmatrix A,
	lm_trans(Path,Ma,[D1,D2],M), !.

% negation
M @@ Path is_lmatrix_p \+((A,[D1,_])) @@ Path :- !,
	Ma is_lmatrix A,
	lm_negate(Path,Ma,D1,M),!.

% equal
M @@ Path is_lmatrix_p M @@ Path:- !, nonvar(M), nonvar(Path).
(M,[D1,D2]) @@ Path is_lmatrix_le (A,[D1,D2]) @@ Path :- !,
    Ma is_lmatrix A,
	Mb is_lmatrix M,
    lm_total_order(Mb,Ma,D1).



% Square of matrices
lm_square(Path,[P,M1],D,[P,M2]) :- M2 is M1*2,
%    lm_trans(Path,[P,M1],[_TP,_TP_M1]),
	lm_prod(Path,[P,M1],[P,M1],D,[P,M2]), !.

lm_square_p(Path,[P,M1],[P,M2]) :- M2 is M1*2,
%    lm_trans_p(Path,[P,M1],[_TP,_TP_M1]),
	lm_prod_p(Path,[P,M1],[P,M1],[P,M2]), !.


% lm_prod/4 - Ms is product of matrices M1,M2
lm_prod(Path,[P1,M1],[P2,M2],D,[P3,M3]) :-
    nonvar(P3),
    (nonvar(M3);M3 is M1+M2),
	mfname(Path,P3,M3,M3name),
	tell(M3name),
	pprod([P1,M1],[P2,M2],D,[P3,M3]),
	told,
	consult(M3name),!.


lm_prod_p(Path,[P1,M1],[P2,M2],[P3,M3]) :-
    (nonvar(M3);M3 is M1+M2),
	pprod_p(Path,[P1,M1],[P2,M2],[P3,M3]),!.


% lm_trans/2 - Generation of Transpose Matrix
lm_trans(Path,[P,M]) :-
    lm_trans(Path,[P,M],_).
% should always specify the dimensions
lm_trans(Path,[P,M],[TP,M]) :-
	(nonvar(TP);mname(t,P,TP)),
	mfname(Path,TP,M,Mname),
	mname(P,M,P_M), mname(TP,M,TP_M),
	tell(Mname),
	% for m x n matrices where m < n
    once((
        setof(U,Z^V^V1^(call(P_M,Z,V),V1 is V + 1,U is msb(V1)),Dim),
        last(Dim,MaxDim),
	    numlist(0,MaxDim,Cols)
    )),
	member(Y,Cols),
	findall(X,(call(P_M,X,Ys),1 is getbit(Ys,Y)),Xs),
	lm_stob1(Xs,BXs),
	write_row_matrix__(TP_M,Y,BXs), fail.
lm_trans(Path,[P,M],[TP,M]) :- !,
	told,
	(nonvar(TP);mname(t,P,TP)),
	mfname(Path,TP,M,Mname),
	consult(Mname).
% use the dimension from matrices
lm_trans(Path,[P,M],[D1,D2],[TP,M]) :-
    (nonvar(TP);mname(t,P,TP)),
    mfname(Path,TP,M,Mname),
	mname(P,M,P_M),
	mname(TP,M,TP_M),
	tell(Mname),
	told,
	dim_list(D1,Rows),
	dim_list(D2,Cols),
	forall(
	    member(Y,Cols),
	    lm_trans_(Mname,P_M,TP_M,Rows,Y)),
	consult(Mname),!.
lm_trans_(Mfname,P_M,TP_M,Rows,Y):-
    findall(X,(member(X,Rows),call(P_M,X,Ys),1 is getbit(Ys,Y)),Xs),
    lm_stob1(Xs,BXs),
    write_row_matrix_(Mfname,TP_M,Y,BXs,append).


% lm_trans_p/2 - parallel version of transpose matrix generation
lm_trans_p(Path,[P,M]) :-
    lm_trans_p(Path,[P,M],_).
lm_trans_p(Path,[P,M],[TP,M]) :-
    (nonvar(TP);mname(t,P,TP)),
    mfname(Path,TP,M,Mname),
	mname(P,M,P_M), mname(TP,M,TP_M),
	tell(Mname),
	told,
	once((
        setof(U,Z^V^V1^(call(P_M,Z,V),V1 is V + 1,U is msb(V1)),Dim),
        last(Dim,MaxDim),
	    numlist(0,MaxDim,Cols)
    )),
	concurrent_forall(
	    member(Y,Cols),
	    lm_trans_p_(Mname,P_M,TP_M,Y)),
	consult(Mname),!.
lm_trans_p_(Mfname,P_M,TP_M,Y):-
    findall(X,(call(P_M,X,Ys),1 is getbit(Ys,Y)),Xs),
    lm_stob1(Xs,BXs),
    write_row_matrix_(Mfname,TP_M,Y,BXs,append).


% lm_prods of list of lmatrices
%lm_prods(_,P,[Depth],[P,Depth]) :- !.
%lm_prods(Path,P,[D|Ds],M) :-
%	lm_prods(Path,P,Ds,M1),
%	M is_lmatrix [P,D] * M1.
%	lm_prod([P,D],M1,M).



% Printing product of lmatrices
% matrices muct be multiplication compatible
% Could optimise meta-level calls
% future work: dedicated helpers for the compiler to optimise

pprod([P1,M1],[P2,M2],D,[P3,M3]) :-
	mname(P1,M1,P_M1), mname(P3,M3,P_M3),
    mname(P2,M2,P_M2),
	dim_list(D,L),
	forall(member(X,L),
	    pprod_(P_M1,P_M2,P_M3,X)).
pprod_(P_M1,_,P_M3,X) :-
	call(P_M1,X,0),!,
    write_row_matrix__(P_M3,X,0).
pprod_(P_M1,P_M2,P_M3,X) :-
	call(P_M1,X,BXs),
	lm_btos1(BXs,BXS),
    findall(BYs,(member(Y,BXS),call(P_M2,Y,BYs)),BYs),
    foldl(or,BYs,0,BYs1),
    write_row_matrix__(P_M3,X,BYs1).

or(A,B,C) :- C is A \/ B.

% Parallel version of pprod (need to not use transpose here)
%pprod_p(Path,[P,M1],[P,M2],[P,M3]) :-
%    (nonvar(TP);mname(t,P,TP)),
%	mname(P,M1,P_M1), mname(TP,M2,TP_M2), mname(P,M3,P_M3),
%	mfname(Path,P,M3,M3fname),
%	tell(M3fname),
%	told,
%	concurrent_forall(
%	    call(P_M1,X,_),
%	    (
%	        pprod_p_(P_M1,TP_M2,X,BYs),
%	        write_row_matrix_(M3fname,P_M3,X,BYs,append)
%	    )
%	),
%	consult(M3fname),!.
%pprod_p_(P_M1,TP_M,X,BYs1) :-
%    call(P_M1,X,BXs),!,
%    findall(Y,(call(TP_M,Y,BYs),BXs/\BYs>0),Ys1),
%	lm_stob1(Ys1,BYs1).


% Add diagonal to a given logical matrix
lm_add_diagonal(Path,[P,M],D,[Pu,M]) :-
	name(P,P1),
	(nonvar(Pu);(name('1U',U1),appends([P1,U1],Pu1))),
	name(Pu,Pu1),
	mfname(Path,Pu,M,Mfname),
	tell(Mfname),
	pdiagonal([P,M],D,[Pu,M]),
	told,
	consult(Mfname), !.



% Printing Matrix with diagonal added
pdiagonal([P,M],D,[Pu1,M]) :-
	mname(P,M,P_M), mname(Pu1,M,Pu1_M),
	dim_list(D,L),
	forall(member(X,L),pdiagonal(P_M,Pu1_M,X)).
pdiagonal(P_M,Pu1_M,X) :-
	call(P_M,X,Y), Y1 is Y\/1<<X,
	write_row_matrix__(Pu1_M,X,Y1).


element(X,[X|_]).
element(X,[_|T]) :- element(X,T).

mfname(Path,P,M,Fname) :-
	atomic_list_concat([Path,P,M],Fname),!.

mname(P,M,Mname) :-
	atomic_concat(P,M,Mname),!.

mnames([Name],Name) :- !.
mnames([H|T],Name) :-
	mnames(T,NameT),
	mname(H,NameT,Name), !.


lm_negate(Path,[P,M],D,[P1,M]) :-
    (nonvar(P1);mname('neg_',P,P1)),
    mname(P,M,P_M),
    mname(P1,M,P_M1),
    mfname(Path,P1,M,M1fname),
    Bits is 1<<D - 1,
    dim_list(D,L),
    tell(M1fname),
	told,
    forall(member(X,L),
           lm_negate_(M1fname,P_M,P_M1,X,Bits)).
lm_negate_(M1fname,P_M,P_M1,X,Bits) :-
    call(P_M,X,Y),
    Y1 is Bits /\ \Y,
    write_row_matrix_(M1fname,P_M1,X,Y1,append).


% lm_eq/3 - test if two matrices are identical
%   in every row
lm_eq(_Path,[P1,M1],[P2,M2]) :-
    mname(P1,M1,PM1), mname(P2,M2,PM2),
    \+((call(PM1,X,Y1),call(PM2,X,Y2),Y1 =\= Y2)).



%lm_eq_p([P,M1],[P,M2]) :-
%    mname(P,M1,PM1), mname(P,M2,PM2),
%    concurrent_forall(
%        (call(PM1,X,Y1),call(PM2,X,Y2)),
%        Y1 == Y2
%    ).


% lm_total_order/3 - test whether every row of 1st matrix is a subset
%   of the corresponding row of the second
lm_total_order([P1,M1],[P2,M2],D) :-
	mname(P1,M1,P_M1), mname(P2,M2,P_M2),
	dim_list(D,L),
	forall(member(X,L),lm_total_order_(P_M1,P_M2,X)).
lm_total_order_(P_M1,P_M2,X) :-
    call(P_M1,X,Y1),call(P_M2,X,Y2),bit_subset_chk(Y1,Y2).



% lm_submatrix/4 - find all rows of the 2nd matrix that is a subset
%   of the Xth row of the 1st matrix
lm_submatrix(X,[P1,M1],[P2,M2],Ns) :-
    mname(P1,M1,P_M1), mname(P2,M2,P_M2),
    lm_submatrix_(X,P_M1,P_M2,Ns).

lm_submatrix_(X,P_M1,P_M2,Ns) :-
    findall(Y,(call(P_M1,X,Y1),call(P_M2,Y,Y2),bit_subset_chk(Y2,Y1)),Ns).

% skip ith row in the 2nd matrix if ith bit in filter is 1
lm_submatrix_f_(X,F,P_M1,P_M2,Ns) :-
    findall(Y,(call(P_M1,X,Y1),call(P_M2,Y,Y2),1 is getbit(F,Y),bit_subset_chk(Y2,Y1)),Ns).

% reverse of lm_submatrix_ operation by switching the 1st and 2nd matrix
lm_submatrix_r_(X,P_M1,P_M2,Ns) :-
    findall(Y,(call(P_M1,X,Y1),call(P_M2,Y,Y2),bit_subset_chk(Y1,Y2)),Ns).



% For all row i in the first matrix, find all indices j of rows
% in the second matrix where the row j contains row i
all_submatrix(Path,[P,M1],[P,M2],[P,M3]) :-
    (nonvar(M3);M3 is M1 + M2),!,
    mname(P,M1,P_M1),
    mfname(Path,P,M3,M3fname),
    tell(M3fname),
	writes(['% Submatrix indices of matrix ',M2,'\n']),
	told,
    forall(call(P_M1,X,_),
            (
                lm_submatrix(X,[P,M1],[P,M2],Rn),
                lm_stob1(Rn,Y),
                write_row_matrix(Path,P,M3,X,Y,append)
            )).


all_submatrix_p(Path,[P,M1],[P,M2],[P,M3]) :-
    (nonvar(M3);M3 is M1 + M2),!,
    mname(P,M1,P_M1),
    mname(P,M2,P_M2),
    mname(P,M3,P_M3),
    mfname(Path,P,M3,M3fname),
    tell(M3fname),
	writes(['% Submatrix indices of matrix ',M2,'\n']),
	told,
    concurrent_forall(
        call(P_M1,X,_),
        all_submatrix_p_(M3fname,P_M1,P_M2,P_M3,X)
    ),
    consult(M3fname),!.
all_submatrix_p_(Mfname,P_M1,P_M2,P_M3,X) :-
    lm_submatrix_(X,P_M1,P_M2,Rn),
    lm_stob1(Rn,Y),
    write_row_matrix_(Mfname,P_M3,X,Y,append).



% For all indices i of rows, subtract row i in the second matrix
% from row i in the first matrix
lm_subtract(Path,[P1,M1],[P2,M2],[P3,M3]) :-
    (nonvar(M3);M3 is M1 + M2),!,
    mname(P1,M1,P_M1),
    mname(P2,M2,P_M2),
    mfname(Path,P3,M3,M3fname),
    tell(M3fname),
	told,
    forall((call(P_M1,X,Y1),call(P_M2,X,Y2)),
            (
                Y3 is Y1 /\ \Y2,
	            write_row_matrix(Path,P3,M3,X,Y3,append)
            )).


lm_subtract_p(Path,[P,M1],[P,M2],[P,M3]) :-
    (nonvar(M3);M3 is M1 + M2),!,
    mname(P,M1,P_M1),
    mname(P,M2,P_M2),
    mname(P,M3,P_M3),
    mfname(Path,P,M3,M3fname),
    tell(M3fname),
	writes(['% Subtract Matrix ',M2,' from Matrix ',M1,'\n']),
	told,
	forall(
%    concurrent_forall(
        (call(P_M1,X,Y1),call(P_M2,X,Y2)),
            (
                Y3 is Y1 /\ \Y2,
	            write_row_matrix_(M3fname,P_M3,X,Y3,append)
            )),
    consult(M3fname),!.



% For all indices i of rows, add row i in the second matrix
% with row i in the first matrix
lm_add(Path,[P1,M1],[P2,M2],[P3,M3]) :-
    (nonvar(P3),nonvar(M3);M3 is M1 + M2),!,
    mname(P1,M1,P_M1),
    mname(P2,M2,P_M2),
    mfname(Path,P3,M3,M3fname),
    tell(M3fname),
	told,
    forall((call(P_M1,X,Y1),call(P_M2,X,Y2)),
            (
                Y3 is Y1 \/ Y2,
                write_row_matrix(Path,P3,M3,X,Y3,append)
            )).

lm_add(Path,[P1,M1],[P2,M2],D,[P3,M3]) :-
    (nonvar(P3),nonvar(M3);M3 is M1 + M2),!,
    mname(P1,M1,P_M1),
    mname(P2,M2,P_M2),
    mname(P3,M3,P_M3),
    dim_list(D,L),
    mfname(Path,P3,M3,M3fname),
    tell(M3fname),
	told,
    forall(member(X,L),
           lm_add_(M3fname,P_M1,P_M2,P_M3,X)).
lm_add_(M3fname,P_M1,P_M2,P_M3,X) :-
    call(P_M1,X,Y1),call(P_M2,X,Y2),
    Y3 is Y1 \/ Y2,
    write_row_matrix_(M3fname,P_M3,X,Y3,append).

lm_add_p(Path,[P,M1],[P,M2],[P,M3]) :-
    (nonvar(M3);M3 is M1 + M2),!,
    mname(P,M1,P_M1),
    mname(P,M2,P_M2),
    mname(P,M3,P_M3),
    mfname(Path,P,M3,M3fname),
    tell(M3fname),
	told,
	forall(
%    concurrent_forall(
            (call(P_M1,X,Y1),call(P_M2,X,Y2)),
            (
                Y3 is Y1 \/ Y2,
                write_row_matrix_(M3fname,P_M3,X,Y3,append)
            )),
    consult(M3fname),!.


% lm_mkcton/1 - make one-one mapping from Herbrand Base to Natural numbers
%	and create auxiliary primitives based on frequent pairings.
lm_mkcton(Cs) :-
	File= 'herbn.pl',
	lm_mkcton(Cs,File).


lm_mkcton(Cs,File) :-
    \+number(File),
	tell(File),
	writes([':- discontiguous(bmlp:cton/2), discontiguous(bmlp:ntoc/2).','\n','\n']),
	lm_mkcton(Cs,0),
	told, consult(File), !.

lm_mkcton([],_) :- !.
lm_mkcton([H|T],N) :-
	portray_clause(bmlp:cton(H,N)), write(' '),
	portray_clause(bmlp:ntoc(N,H)), nl,
	N1 is N+1,
	lm_mkcton(T,N1), !.

lm_mkcton_mul(Ps,File) :-
    \+number(File),
	tell(File),
	writes([':- discontiguous(bmlp:cton/3), discontiguous(bmlp:ntoc/3).','\n','\n']),
	lm_mkcton_mul_(Ps,File),
	told, consult(File), !.

lm_mkcton_mul_([],_) :- !.
lm_mkcton_mul_([(P,Cs)|T],File) :-
	lm_mkcton(P,Cs,0),
	lm_mkcton_mul_(T,File).

lm_mkcton(_,[],_) :- !.
lm_mkcton(P,[H|T],N) :-
	portray_clause(bmlp:cton(P,H,N)), write(' '),
	portray_clause(bmlp:ntoc(P,N,H)), nl,
	N1 is N+1,
	lm_mkcton(P,T,N1), !.


% lm_stob/2 - convert a subset of the Herbrand Base to a Bitset
lm_stob(Set,Bitset) :-
	lm_stob(Set,0,Bitset).

lm_stob([],Bs,Bs) :- !.
lm_stob([H|T],Bs1,Bs2) :-
	cton(_,H,N),
	Bs3 is Bs1\/1<<N,
	lm_stob(T,Bs3,Bs2), !.


% lm_stob1/2 - convert a set of Numbers to a Bitset
lm_stob1(NSet,Bitset) :-
    set_max_integer_size,
	lm_stob1(NSet,0,Bitset).

lm_stob1([],Bs,Bs) :- !.
lm_stob1([N|T],Bs1,Bs2) :-
   N < 0,!,
   lm_stob1(T,Bs1,Bs2).
lm_stob1([N|T],Bs1,Bs2) :-
	Bs3 is Bs1\/1<<N,
	lm_stob1(T,Bs3,Bs2), !.


% lm_btos/2 - convert a Bitset to a subset of the Herbrand Base
lm_btos(P,Bitset,Set) :-
    set_max_integer_size,
	lm_btos(P,Bitset,[],Set).

lm_btos(_,0,Set,Set) :- !.
lm_btos(P,Bs1,Set1,[H|Set2]) :-
	N is lsb(Bs1), ntoc(P,N,H),
	Bs2 is Bs1/\ \(1<<N),
	lm_btos(P,Bs2,Set1,Set2), !.


% lm_btos1/2 - convert a Bitset to a set of Numbers
lm_btos1(Bitset,Set) :-
    set_max_integer_size,
	lm_btos1(Bitset,[],Set).

lm_btos1(0,Set,Set) :- !.
lm_btos1(Bs1,Set1,[N|Set2]) :-
	N is lsb(Bs1),
	Bs2 is Bs1/\ \(1<<N),
	lm_btos1(Bs2,Set1,Set2), !.


% a list of 0 and 1s to a bitcode (as an integer)
lm_ltob(Bl,Bs) :-
    set_max_integer_size,
    lm_ltob(Bl,0,Bs).
lm_ltob([1],Bs1,Bs2) :- !,
    Bs2 is Bs1 + 1.
lm_ltob([0],Bs,Bs) :- !.
lm_ltob([B|Bl],Bs1,Bs2) :-
    Bs3 is (Bs1 \/ B) << 1,
    lm_ltob(Bl,Bs3,Bs2),!.


% a bitcode (as an integer) to a list of 0 and 1s
lm_btol(Bs,D,Bl) :-
    set_max_integer_size,
    dim_list(D,L),
    lm_btol_(Bs,L,Bl).
lm_btol_(Bs,L,Bl) :-
    lm_btos1(Bs,S),
    findall(B,(member(I,L),(memberchk(I,S) -> B=1;B=0)),Bl).


count_ones(Bs,N) :-
    set_max_integer_size,
    count_ones(Bs,0,N).
count_ones(0,N,N) :- !.
count_ones(Bs,N1,N2) :-
    1 is Bs /\ 1,!,
    Bs1 is Bs >> 1,
    N3 is N1 + 1,
    count_ones(Bs1,N3,N2).
count_ones(Bs,N1,N2) :-
    Bs1 is Bs >> 1,
    count_ones(Bs1,N1,N2).


% reverse a bitset given a bitset max length
reverse_bs(_,0,0) :- !.
reverse_bs(M,Bs1,Bs2) :-
    lm_btos1(Bs1,S1),
    findall(N2,(member(N1,S1),N2 is M-1-N1),S2),
    lm_stob1(S2,Bs2).


% reverse a bitset
reverse_bs(0,0) :- !.
reverse_bs(Bs1,Bs2) :-
    lm_btos1(Bs1,S1),
    L is msb(Bs1),
    findall(N2,(member(N1,S1),N2 is L-N1),S2),
    lm_stob1(S2,Bs2).

dim_list(D,L) :-
    RowDim is D - 1,
    numlist(0,RowDim,L).

lm_loaded([P,M]) :-
    mname(P,M,P_M),
    current_predicate(P_M/2).
lm_loaded(matrix([P,M],_,_,_)) :-
    mname(P,M,P_M),
    current_predicate(P_M/2).

% load a matrix if it has not been loaded
lm_consult([P,M]) :-
    (lm_loaded([P,M]) -> true;
    (srcPath(BasePath),
    mfname(BasePath,P,M,F),
    consult(F))).
lm_consult(matrix(M,_,_,_)) :-
    (lm_loaded(M) -> true;
    (srcPath(BasePath),
    atomic_list_concat([BasePath|M],F),
    consult(F))).


% unload a matrix
lm_unload([P,M]) :-
    srcPath(BasePath),
    mfname(BasePath,P,M,F),
    unload_file(F).
lm_unload(matrix(M,_,_,_)) :-
    srcPath(BasePath),
    atomic_list_concat([BasePath|M],F),
    unload_file(F).


% print a matrix given a matrix term
% require the dimensions of matrix
lm_print(M) :-
    lm_consult(M),
    lm_print_(M).
lm_print_(matrix(M,[T1,T2],[D1,D2],_)) :-
    atomic_list_concat(M,Mn),
    format('~w (~wx~w):\n',[Mn,D1,D2]),
    findall(C,cton(T2,C,_),Cs),
    atomics_to_string(Cs,' ',S),
    format('\t ~w\n',[S]),
    dim_list(D1,L),
    forall(member(X,L),lm_print__(Mn,T1,D2,X)).
lm_print__(P,T,D,X) :-
    call(P,X,Y),
    cton(T,C,X),
    lm_btol(Y,D,L),
    atomics_to_string(L,' ',A),
    format('~w\t|~w|\n',[C,A]).

% convert a boolean matrix into a list of facts
lm_to_facts(matrix([P,N],[T,T],[D,D],_),Fs) :- !,
    lm_consult([P,N]),
    mname(P,N,P_N),
    srcPath(BasePath),
    mfname(BasePath,T,'_csmap.pl',Fn),
    consult(Fn),
    dim_list(D,L),
    findall(Ts,(member(X,L),lm_to_facts_(P,P_N,T,T,X,Ts)),Tss),
    append(Tss,Fs).
lm_to_facts(matrix([P,N],[T1,T2],[D1,_],_),Fs) :-
    lm_consult([P,N]),
    mname(P,N,P_N),
    srcPath(BasePath),
    atomic_list_concat([BasePath,T1,'_',T2,'_csmap.pl'],Fn),
    consult(Fn),
    dim_list(D1,L),
    findall(Ts,(member(X,L),lm_to_facts_(P,P_N,T1,T2,X,Ts)),Tss),
    append(Tss,Fs).
lm_to_facts_(P,P_N,T1,T2,X,Ts) :-
    ntoc(T1,X,C),
    call(P_N,X,Y),
    lm_btos(T2,Y,S),
    findall(T,(member(C1,S),T=..[P,C,C1]),Ts).