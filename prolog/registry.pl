:- module(registry, [
    new/2,
    delete/1,
    next_rr/2,
    options/2,
    connections/2,
    dead_connections/2
]).

:- initialization(mutex_create(_, [alias(plasticsearch)]), restore).

new(Key, Dict) :-
    build_var_dict(Dict, Vars),
    recorda(Key, Dict.put(vars, Vars)).

delete(Key) :-
    recorded(Key, _, Ref),
    erase(Ref).

build_var_dict(Dict, Vars) :-
    Vars0 = _{connections:Dict.hosts, dead_connections:[]},
    (   memberchk(random_selector(false), Dict.options)
    ->  Vars = Vars0.put(rr, -1)
    ;   Vars = Vars0
    ).

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
