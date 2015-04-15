:- module(connections_pool, [
    get_connection/2,
    mark_alive/2,
    mark_dead/2
]).

:- use_module(library(random)).
:- use_module(library(lists)).

:- use_module(registry).
:- use_module(util).

get_connection(Ps, Connection) :-
    with_mutex(Ps, (
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
            ;   random(Connections, Connection)
            )
        )
    )).

next_rr(Ps, RR) :-
    recorded(Ps, Value, Ref),
    erase(Ref),
    RR is Value.vars.rr + 1,
    recorda(Ps, Value.put([vars=Value.vars.put(rr, RR)])).

mark_alive(Ps, Connection) :-
    with_mutex(Ps, (
        recorded(Ps, Value, Ref),
        erase(Ref),
        DeadCount = Value.vars.dead_count,
        (   selectchk(Connection-_, DeadCount, DeadCount1)
        ->  true
        ;   DeadCount1 = DeadCount
        ),
        recorda(Ps, Value.put([vars=Value.vars.put(dead_count, DeadCount1)]))
    )).

mark_dead(Ps, Connection) :-
    with_mutex(Ps, (
        recorded(Ps, Value, Ref),
        erase(Ref),
        Connections = Value.vars.connections,
        (   selectchk(Connection, Connections, Connections1)
        ->  update_connection_info(Ps, Connection, Connections1, Value)
        ;   true
        )
    )).

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
    keysort(DeadConnections1, DeadConnections2),
    Value1 = Value.put(vars, Value.vars.put(_{
        connections:Connections,
        dead_connections:DeadConnections2,
        dead_count:[Connection-Count1|DeadCount1]})),
    recorda(Ps, Value1).

resurrect(Ps, Force, Connection) :-
    with_mutex(Ps, (
        recorded(Ps, Value, Ref),
        erase(Ref),
        _{connections:Connections, dead_connections:DeadConnections} :< Value.vars,
        (   DeadConnections = []
        ->  (   Force
            ->  random(Value.hosts, Connection)
            ;   true
            ),
            Value1 = Value
        ;   DeadConnections = [Timeout-Connection|DeadConnections1],
            (   once((get_time(Now), Timeout =< Now; Force))
            ->  Connections1 = [Connection|Connections],
                Value1 = Value.put(vars, Value.vars.put(_{
                    connections:Connections1, dead_connections:DeadConnections1}))
            ;   Value1 = Value
            )
        ),
        recorda(Ps, Value1)
    )).
