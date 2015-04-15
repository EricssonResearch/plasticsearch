:- module(connections_pool, [
    get_connection/2
]).

:- use_module(library(random)).
:- use_module(library(lists)).

:- use_module(registry).

get_connection(Ps, Connection) :-
    options(Ps, Options),
    memberchk(random_selector(false), Options), !,
    next_rr(Ps, RR),
    connections(Ps, Connections),
    length(Connections, Length),
    Index is RR mod Length,
    nth0(Index, Connections, Connection).

get_connection(Ps, Connection) :-
    options(Ps, Options),
    memberchk(random_selector(true), Options), !,
    connections(Ps, Connections),
    length(Connections, Length),
    random_between(0, Length, Index),
    nth0(Index, Connections, Connection).
