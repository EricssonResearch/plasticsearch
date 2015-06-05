:- module(registry, [
    new/2,              % +Key, +Dict
    delete/1,           % +Key
    options/2,          % +Key, -Options
    connections/2       % +Key, -Options
]).

/** <module> Registry of Plasticsearch instances

@auther Hongxin Liang
@license TBD
*/

%% new(+Key, +Dict) is det.
%
% Register a new Plasticsearch instance.

new(Key, Dict) :-
    mutex_create(_, [alias(Key)]),
    build_var_dict(Dict, Vars),
    Value = Dict.put(vars, Vars),
    debug(registry, 'register a new plasticsearch ~w', [Value]),
    recorda(Key, Value).

%% delete(+Key) is semidet.
%
% Delete a Plasticsearch instance.

delete(Key) :-
    recorded(Key, Value, Ref),
    debug(registry, 'deregister plasticsearch ~w', [Value]),
    erase(Ref),
    mutex_destroy(Key).

build_var_dict(Dict, Vars) :-
    Vars0 = _{connections:Dict.hosts, dead_connections:[], dead_count:[]},
    (   memberchk(random_selector(false), Dict.options)
    ->  Vars = Vars0.put(rr, -1)
    ;   Vars = Vars0
    ).

%% options(+Key, -Options) is semidet.
%
% Get options of a Plasticsearch instance.

options(Key, Options) :-
    with_mutex(Key, (
        recorded(Key, Value, _),
        Options = Value.options)
    ).

%% connections(+Key, -Connections) is semidet.
%
% Get connections of a Plasticsearch instance.

connections(Key, Connections) :-
    with_mutex(Key, (
        recorded(Key, Value, _),
        Connections = Value.vars.connections)
    ).
