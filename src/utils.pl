%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%	Utilities
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% SWI-Pl 9.2 requires setting max_integer byte size
% default max integer binary shift size is 2175
% when exceeding this bound an resource error is given

default(max_integer_size(100000)).
%:- set_prolog_flag(stack_limit,16000000000).

set_max_integer_size :-
    current_predicate(max_integer_size/1),!,
    max_integer_size(M),
    set_prolog_flag(max_integer_size,M).
set_max_integer_size :-
    default(max_integer_size(M)),!,
    set_prolog_flag(max_integer_size,M).

expand_abd(abd(_,[[]]),[]) :- !.
expand_abd(abd(_,AbdEncap),Abds) :-
    findall(Abd,(member(AbdTerm,AbdEncap),Abd=..AbdTerm,nonvar(Abd)),Abds).

abd_sym(I,NewAbdSym) :-
    atom_concat(abdsym,I,NewAbdSym),!.

pp_list(L,Pre,Sep,Post,Pr) :-
	pp_sep_list(L,Sep,Pr1),
	appends([Pre,Pr1,Post],Pr), !.

pp_sep_list([],_,[]) :- !.
pp_sep_list([X],_,[X]) :- !.
pp_sep_list([H|T],Sep,Pr) :-
	pp_sep_list(T,Sep,Rest),
	appends([[H],Sep,Rest],Pr), !.

appends([],[]).
appends([H|T],R) :-
	appends(T,R1),
	append(H,R1,R).

% append([],L,L).
% append([H|T],L,[H|I]) :-
%	append(T,L,I).

%member(X,[X|_]).
%member(X,[_|T]) :-
%	member(X,T).

% not(X) :- X, !, fail.
% not(_) :- !.

writes([H|T]) :-
	mywrite(H),
	writes(T), !.
writes([]) :- !.

writes(Stream,[H|T]) :-
	mywrite(Stream,H),
	writes(Stream,T), !.
writes(_,[]) :- !.

mywrite(V) :- var(V), write(V), !.
mywrite('\n') :- nl, !.
mywrite('\t') :- write('	'), !.
mywrite(X) :- write(X), !.

mywrite(Stream,V) :- var(V), write(Stream,V), !.
mywrite(Stream,'\n') :- nl(Stream), !.
mywrite(Stream,'\t') :- write(Stream,'	'), !.
mywrite(Stream,X) :- write(Stream,X), !.

%
% bit_subset/2 - subset test for bitsets
%

bit_subset(Bs1,Bs2) :- Bs1 is Bs1/\Bs2.

bit_subset_chk(Bs1,Bs2) :- Bs1 =:= Bs1/\Bs2.

%
% bit_element/2 - element test for bitsets
%

bit_element(B,Bs) :- nonvar(B), nonvar(Bs), 1 is getbit(Bs,B).
bit_element(B,Bs) :- var(B), nonvar(Bs), bit_element1(B,Bs).

bit_element1(_,0) :- !, fail.
bit_element1(B,Bs) :- B is lsb(Bs).
bit_element1(B,Bs) :- N is lsb(Bs), Bs1 is Bs>>(N+1),
	bit_element1(B1,Bs1), B is B1+N+1.

%
% bit_call/2 - call binary predicate P(X,Y) where X is encoded as a number
%	and Y is encoded as a bitset
%

bit_call(P,X,Y) :-
        cton(X,BX), call(P,BX,BYs), bit_element(BY,BYs), ntoc(BY,Y), !.

%
% set_subset/2 - subset test
%

set_subset([],_) :- !.
set_subset([H1|S1],[H2|S2]) :- H1 == H2, set_subset(S1,S2), !.
set_subset(S1,[_|S2]) :- !, set_subset(S1,S2), !.

%
% set_sub/3 - set subtraction for ordered sets
%

set_sub(X,[],X) :- !.  set_sub([],_,[]) :- !.
set_sub([H1|T1],[H2|T2],L) :- H1 == H2, set_sub(T1,T2,L), !.	% Equal
set_sub([H1|T1],[H2|T2],L) :- H1 @> H2, set_sub([H1|T1],T2,L), !. % Greater
set_sub([H|T],S,[H|L]) :- set_sub(T,S,L), !.			% Less

%
% set_uni/3 - set union for ordered sets
%
set_unis([],[]) :- !.
set_unis([S1|Ss],S2) :-
	set_unis(Ss,S3),
	set_uni(S1,S3,S2), !.

set_uni([],S2,S2) :- !.		  set_uni(S1,[],S1) :- !.
set_uni([H1|T1],[H2|T2],[H1|T3]) :-	% Equal
	H1 == H2,
	set_uni(T1,T2,T3), !.
set_uni([H1|T1],[H2|T2],[H2|T3]) :-	% Greater than
	H1 @> H2, !,
	set_uni([H1|T1],T2,T3), !.
set_uni([H1|T1],S2,[H1|T3]) :- !,	% Less than
	set_uni(T1,S2,T3), !.

%
% set_int/3 - set intersection for ordered sets
%

set_int([],_,[]) :- !.	  set_int(_,[],[]) :- !.
set_int([H1|T1],[H2|T2],[H1|T3]) :-	% Equal
	H1 == H2,
	set_int(T1,T2,T3), !.
set_int([H1|T1],[H2|T2],T3) :-		% Greater than
	H1 @> H2, !,
	set_int([H1|T1],T2,T3), !.
set_int([_|T1],S2,T3) :- !,		% Less than
	set_int(T1,S2,T3), !.


%
% set_int_uni/4 - Simultaneous set intersection and union for ordered sets
%

set_int_uni([],S2,[],S2) :- !.		  set_int_uni(S1,[],[],S1) :- !.
set_int_uni([H1|T1],[H2|T2],[H1|T3],[H1|T4]) :-	% Equal
	H1 == H2,
	set_int_uni(T1,T2,T3,T4), !.
set_int_uni([H1|T1],[H2|T2],T3,[H2|T4]) :-	% Greater than
	H1 @> H2, !,
	set_int_uni([H1|T1],T2,T3,T4).
set_int_uni([H1|T1],S2,T3,[H1|T4]) :- !,	% Less than
	set_int_uni(T1,S2,T3,T4), !.

%
% List product appends all pairs of lists together
%

set_prod(Ll1,Ll2,Ll3) :-
	setof(	Paired,
		join_pair(Ll1,Ll2,Paired),
		Ll3), !.

join_pair(Ll1,Ll2,Paired) :-
	member(I1,Ll1),
	member(I2,Ll2),
	set_uni(I1,I2,Paired).

:- dynamic '%%bagstuff'/1.

mybagof(X,P,_) :-
	retractall(('%%bagstuff'(_) :- !)),
	assert(('%%bagstuff'([]) :- !)),
	P,
	'%%bagstuff'(Bag1),
	replace(('%%bagstuff'(Bag1) :- !),('%%bagstuff'([X|Bag1]) :- !)),
	fail, !.
mybagof(_,_,Bag) :-
	'%%bagstuff'(Bag1),
	reverse(Bag1,Bag), !.

replaceall(Old,New) :-
	retractall(Old),
	assert(New), !.

replace(Old,New) :-
	retract(Old),
	assert(New), !.

reverse(List,RevList) :- !,
	reverse(List,[],RevList).

reverse([H|T],SoFar,RevList) :- !,
	reverse(T,[H|SoFar],RevList).
reverse([],RevList,RevList) :- !.

mysetof(X,P,Z) :-
	setof(X,P,Z), !.
mysetof(_,_,[]) :- !.

lst_sub(UVW,V,UW) :-
	append(U,V,UV),
	append(UV,W,UVW),
	append(U,W,UW).

mylength([],0).
mylength([_|T],N) :-
	Nm1 is N - 1,
	mylength(T,Nm1).

% may use built-in time/1 of swi-prolog
%time(X) :- T1 is cputime, call(X), T is cputime-T1,
%	writes([X,' - time taken ',T,' seconds.','\n']).

% merge sort with repetition

msort([],[]) :- !.
msort([X],[X]) :- !.
msort(L1,L2) :-
	split(L1,L11,L12),
	msort(L11,L21),
	msort(L12,L22),
	merge(L21,L22,L2).

split([],[],[]).
split([H|L],[H|L1],L2) :-
	split(L,L2,L1).

merge([],S2,S2) :- !.		  merge(S1,[],S1) :- !.
merge([H1|T1],[H2|T2],[H1,H2|T3]) :-	% Equal
	H1 == H2,
	merge(T1,T2,T3), !.
merge([H1|T1],[H2|T2],[H2|T3]) :-	% Greater than
	H1 @> H2, !,
	merge([H1|T1],T2,T3), !.
merge([H1|T1],S2,[H1|T3]) :- !,	% Less than
	merge(T1,S2,T3), !.

suffix(L,L).
suffix([_|T],S) :- suffix(T,S).

% Count number of each element in a sorted bag

countbag([],[]) :- !.
countbag([H|T],Counted) :-
	countbag(H,T,1,Counted).

countbag(C,[],N,[C^N]) :- !.
countbag(C,[C|T],N,Rest) :-
	N1 is N+1,
	countbag(C,T,N1,Rest), !.
countbag(C,[D|T],N,[C^N|Rest]) :-
	C\=D,
	countbag(D,T,1,Rest), !.

% Maximum of two numbers

max(N,M,N) :- number(N), number(M), N>M.
max(N,M,M) :- number(N), number(M).

% N dec places

decpl(R,N,R1) :-
	R1 is ceiling(R*10^N)/10^N , !.


sum_elements_in_list(L,Sum):-
	foldl(my_plus,L,0,Sum), !.

my_plus(A,B,C) :-
    C is A + B.


get_arg(Args,Key,Value) :-
    member((Key=Value),Args),!.

% skip if term is T
write_fact('') :- !.
write_fact(P) :-
    writeq(P),
    writeln(".").

write_fact(Stream,P) :-
    writeq(Stream,P),
    writeln(Stream,".").

write_neg_fact(Stream,P) :-
    write(Stream,":-"),
    writeq(Stream,P),
    writeln(Stream,".").

write_facts(Ps) :-
    forall(member(P,Ps),write_fact(P)).
write_facts(Stream,Ps) :-
    forall(member(P,Ps),write_fact(Stream,P)).
write_facts(Stream,Ps,F) :-
    forall(member(P,Ps),call(F,Stream,P)).

ns :-write(' ').

write_l([]) :-
    write(']').
write_l([W]) :-
    write('\''),write(W),write('\''),write(']'),!.
write_l([W|Ws]) :-
    write('\''),write(W),write('\''),write(','),
    write_l(Ws).

write_list(W) :-
    write('['),write_l(W).

write_facts_to_file(Facts,Path) :-
    tell(Path),
    write_facts(Facts),
    told.