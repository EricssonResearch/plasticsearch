:- module(registry, [
    new/2,
    delete/1,
    next_rr/2,
    options/2,
    connections/2,
    dead_connections/2
]).

:- use_module(library(lists)).
:- use_module(library(apply)).

:- initialization(mutex_create(_, [alias(plasticsearch)]), restore).

new(Key, Dict) :-
    build_var_dict(Dict, Vars),
    recorda(Key, Dict.put(vars, Vars)).

delete(Key) :-
    recorded(Key, _, Ref),
    erase(Ref).

build_var_dict(Dict, Vars) :-
    Vars0 = _{connections:Dict.hosts, dead_connections:[], dead_count:_{}},
    (   memberchk(random_selector(false), Dict.options)
    ->  Vars = Vars0.put(rr, -1)
    ;   Vars = Vars0
    ).

mark_alive(Key, Connection) :-
    with_mutex(plasticsearch, (
        recorded(Key, Value, Ref),
        erase(Ref),
        DeadCount = Value.vars.dead_count,
        (   del_dict(Connection, DeadCount, _, DeadCount1)
        ->  true
        ;   DeadCount1 = DeadCount
        ),
        recorda(Key, Value.put([vars=Value.vars.put(dead_count, DeadCount1)])))
    ).

mark_dead(Key, Connection) :-
    with_mutex(plasticsearch, (
        recorded(Key, Value, Ref),
        erase(Ref),
        Connections = Value.vars.connections,
        (   selectchk(Connection, Connections, Connections1)
        ->  update_dead_info(Key, Connection, Connections1, Value)
        ;   true
        )
    )).

update_dead_info(Key, Connection, Connections, Value) :-
    _{dead_connections:DeadConnections, dead_count:DeadCountDict} :< Value.vars,
    (   DeadCount = DeadCountDict.get(Connection)
    ->  true
    ;   DeadCount = 0
    ),
    DeadCount1 is DeadCount + 1,
    memberchk(dead_timeout(DeadTimeout), Value.options),
    memberchk(timeout_cutoff(TimeoutCutoff), Value.options),
    Timeout is DeadTimeout * 2 ** min(DeadCount - 1, TimeoutCutoff),
    get_time(Now),
    Future is Now + Timeout,
    DeadConnections1 = [Future-Connection|DeadConnections],
    keysort(DeadConnections1, DeadConnections2),
    Value1 = Value.put(vars, Value.vars.put(_{
        connections:Connections,
        dead_connections:DeadConnections2,
        dead_count:DeadCountDict.put(Connection, DeadCount1)})),
    recorda(Key, Value1).

next_rr(Key, RR) :-
    with_mutex(plasticsearch, (
        recorded(Key, Value, Ref),
        erase(Ref),
        RR is Value.vars.rr + 1,
        recorda(Key, Value.put([vars=Value.vars.put(rr, RR)])))
    ).

options(Key, Options) :-
    with_mutex(plasticsearch, (
        recorded(Key, Value, _),
        Options = Value.options)
    ).

connections(Key, Connections) :-
    with_mutex(plasticsearch, (
        recorded(Key, Value, _),
        Connections = Value.vars.connections)
    ).

dead_connections(Key, DeadConnections) :-
    with_mutex(plasticsearch, (
        recorded(Key, Value, _),
        DeadConnections = Value.vars.dead_connections)
    ).
