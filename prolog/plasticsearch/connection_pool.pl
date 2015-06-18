:- module(connection_pool, [
    get_connection/2,       % +Ps, -Connection
    mark_alive/2,           % +Ps, +Connection
    mark_dead/2             % +Ps, +Connection
]).

/** <module> Connection pool.
Connection pool manages lifecycle of connections.

@author Hongxin Liang
@license Apache License Version 2.0
*/

:- use_module(library(random)).
:- use_module(library(lists)).

:- use_module(registry).
:- use_module(util).

%% get_connection(+Ps, -Connections) is det.
%
% Get one connection for pool. If there is no alive
% connection, a randomly selected one will be returned.

get_connection(Ps, Connection) :-
    with_mutex(Ps, get_connection0(Ps, Connection)).

get_connection0(Ps, Connection) :-
    resurrect(Ps, false, _),
    connections(Ps, Connections),
    (   Connections = []
    ->  resurrect(Ps, true, Connection)
    ;   options(Ps, Options),
        (   memberchk(random_selector(false), Options)
        ->  next_rr(Ps, RR),
            length(Connections, Length),
            Index is RR mod Length,
            nth0(Index, Connections, Connection)
        ;   random_select(Connection, Connections, _)
        )
    ).

next_rr(Ps, RR) :-
    recorded(Ps, Value, Ref),
    erase(Ref),
    RR is Value.vars.rr + 1,
    recorda(Ps, Value.put(vars, Value.vars.put(rr, RR))).

%% mark_alive(+Ps, +Connection) is det.
%
% Mark a connection as alive.

mark_alive(Ps, Connection) :-
    with_mutex(Ps, mark_alive0(Ps, Connection)).

mark_alive0(Ps, Connection) :-
    recorded(Ps, Value, Ref),
    erase(Ref),
    DeadCount = Value.vars.dead_count,
    (   selectchk(Connection-_, DeadCount, DeadCount1)
    ->  true
    ;   DeadCount1 = DeadCount
    ),
    recorda(Ps, Value.put([vars=Value.vars.put(dead_count, DeadCount1)])).

%% mark_dead(+Ps, +Connection) is det.
%
% Mark a connection as dead.

mark_dead(Ps, Connection) :-
    with_mutex(Ps, mark_dead0(Ps, Connection)).

mark_dead0(Ps, Connection) :-
    recorded(Ps, Value, Ref),
    erase(Ref),
    Connections = Value.vars.connections,
    (   selectchk(Connection, Connections, Connections1)
    ->  update_connection_info(Ps, Connection, Connections1, Value)
    ;   true
    ).

update_connection_info(Ps, Connection, Connections, Value) :-
    _{dead_connections:DeadConnections, dead_count:DeadCount} :< Value.vars,
    (   selectchk(Connection-Count, DeadCount, DeadCount1)
    ->  true
    ;   Count = 0
    ),
    Count1 is Count + 1,
    memberchk(dead_timeout(DeadTimeout), Value.options),
    memberchk(timeout_cutoff(TimeoutCutoff), Value.options),
    Timeout is DeadTimeout * 2 ** min(Count1 - 1, TimeoutCutoff),
    get_time(Now),
    Future is Now + Timeout,
    DeadConnections1 = [Future-Connection|DeadConnections],
    debug(connection_pool,
        'mark connect ~w dead with timeout ~w and count ~w', [Connection, Timeout, Count1]),
    keysort(DeadConnections1, DeadConnections2),
    Value1 = Value.put(vars, Value.vars.put(_{
        connections:Connections,
        dead_connections:DeadConnections2,
        dead_count:[Connection-Count1|DeadCount1]})),
    recorda(Ps, Value1).

resurrect(Ps, Force, Connection) :-
    with_mutex(Ps, resurrect0(Ps, Force, Connection)).

resurrect0(Ps, Force, Connection) :-
    recorded(Ps, Value, Ref),
    erase(Ref),
    _{connections:Connections, dead_connections:DeadConnections} :< Value.vars,
    (   DeadConnections = []
    ->  (   Force
        ->  random_select(Connection, Value.hosts, _),
            debug(connection_pool,
                'forced to resurrect, choose a random one ~w', [Connection])
        ;   true
        ),
        Value1 = Value
    ;   DeadConnections = [Timeout-Connection|DeadConnections1],
        (   once((get_time(Now), Timeout =< Now; Force))
        ->  Connections1 = [Connection|Connections],
            debug(connection_pool,
                'dead timeout or forced ~w', [Connection]),
            Value1 = Value.put(vars, Value.vars.put(_{
                connections:Connections1, dead_connections:DeadConnections1}))
        ;   Value1 = Value
        )
    ),
    recorda(Ps, Value1).
