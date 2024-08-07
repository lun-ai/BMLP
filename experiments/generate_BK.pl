:- ['bmlp/utils.pl'].
:- use_module(library(random)).

:- set_prolog_flag(stack_limit, 16000000000).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% constants and background generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_background(PE,N,Path) :-
    get_constants(N,Cs,AllNodes),
    reverse(Cs,CssRev),
    get_routes(PE,Cs,[],AllConnections1),
    get_routes(PE,CssRev,[],AllConnections2),
    append(AllConnections1,AllConnections2,AllConnections3),
    sort(AllConnections3,ConnectionsNoDupl),
    atom_concat(Path,'/background.pl',BK),
    tell(BK),
    write_facts(AllNodes),
    write_facts(ConnectionsNoDupl),
    told.

conversion_background_to_dl(Path) :-
    atom_concat(Path,'/background.pl',BK),
    atom_concat(Path,'/edge.facts',Facts),
    consult(BK),
    tell(Facts),
    forall(
        node(C1),
        (
            atom_string(C1,S1),
            write_fact(node(S1))
        )
    ),
    forall(
        edge(C1,C2),
        (
            atom_string(C1,S1),
            atom_string(C2,S2),
            format('~w\t~w\n',[S1,S2])
        )
    ),
    told.

get_constants(0,[],[]).
get_constants(N,[C|L1],[node(C)|T1]) :- !,
    N1 is N - 1,
    atom_concat(c,N,C),
    get_constants(N1,L1,T1).

get_routes(_,[],Connections,Connections).
get_routes(PE,[C|Cs],Connections1,Connections2) :- !,
    get_routes_(PE,C,Cs,Connections),
    append(Connections,Connections1,Connections3),
    get_routes(PE,Cs,Connections3,Connections2).
get_routes_(_,_,[],[]).
get_routes_(PE,C1,[C2|Cs],[edge(C1,C2)|Connections]) :-
    random(RandFloat),
    RandFloat =< PE,!,
    get_routes_(PE,C1,Cs,Connections).
get_routes_(PE,C1,[_|Cs],Connections) :- !,
    get_routes_(PE,C1,Cs,Connections).