:- module(registry, [
    new/2,
    delete/1,
    options/2,
    connections/2
]).

new(Key, Dict) :-
    mutex_create(_, [alias(Key)]),
    build_var_dict(Dict, Vars),
    recorda(Key, Dict.put(vars, Vars)).

delete(Key) :-
    recorded(Key, _, Ref),
    erase(Ref),
    mutex_destroy(Key).

build_var_dict(Dict, Vars) :-
    Vars0 = _{connections:Dict.hosts, dead_connections:[], dead_count:[]},
    (   memberchk(random_selector(false), Dict.options)
    ->  Vars = Vars0.put(rr, -1)
    ;   Vars = Vars0
    ).

options(Key, Options) :-
    with_mutex(Key, (
        recorded(Key, Value, _),
        Options = Value.options)
    ).

connections(Key, Connections) :-
    with_mutex(Key, (
        recorded(Key, Value, _),
        Connections = Value.vars.connections)
    ).
