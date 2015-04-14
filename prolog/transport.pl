:- module(transport, [
    perform_request/6,
    perform_request/7
]).

:- use_module(library(uri)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(registry)).

:- debug(transport).

perform_request(Ps, get, Context, Params, Status, Reply) :- !,
    compose_url(Ps, Context, Params, URL),
    http_get(URL, Reply, [status_code(Status), json_object(dict)]),
    (   Status >= 200, Status < 300
    ->  true
    ;   throw(http_exception(Status, Reply))
    ).

perform_request(Ps, delete, Context, Params, Status, Reply) :-
    compose_url(Ps, Context, Params, URL),
    http_delete(URL, Reply, [status_code(Status), json_object(dict)]),
    (   Status >= 200, Status < 300
    ->  true
    ;   throw(http_exception(Status, Reply))
    ).

perform_request(Ps, post, Context, Params, Body, Status, Reply) :- !,
    compose_url(Ps, Context, Params, URL),
    http_post(URL, json(Body), Reply, [status_code(Status), json_object(dict)]),
    (   Status >= 200, Status < 300
    ->  true
    ;   throw(http_exception(Status, Reply))
    ).

perform_request(Ps, put, Context, Params, Body, Status, Reply) :-
    compose_url(Ps, Context, Params, URL),
    http_put(URL, json(Body), Reply, [status_code(Status), json_object(dict)]),
    (   Status >= 200, Status < 300
    ->  true
    ;   throw(http_exception(Status, Reply))
    ).

compose_url(Ps, Context, Params, URL) :-
    get_host(Ps, uri_components(Scheme, Authority, Path, Search, _)),
    atom_concat(Path, Context, NewPath),
    dict_pairs(Params, _, Pairs),
    uri_query_components(Search, Pairs),
    uri_components(URL, uri_components(Scheme, Authority, NewPath, Search, _)).

get_host(plasticsearch(id(ID), hosts(Hosts), options(Options), vars(Vars)), Host) :-
    memberchk(random_selector(false), Options), !,
    (   Value = Vars.get(rr)
    ->  true
    ;   Value = -1
    ),
    NewValue is Value + 1,
    safe_update(ID, plasticsearch(id(ID), hosts(Hosts), options(Options), vars(Vars.put(rr, NewValue)))),
    length(Hosts, Length),
    Index is NewValue mod Length,
    nth0(Index, Hosts, Host).

get_host(plasticsearch(_, hosts(Hosts), options(Options), vars(_)), Host) :-
    memberchk(random_selector(true), Options), !,
    length(Hosts, Length),
    random_between(0, Length, Index),
    nth0(Index, Hosts, Host).
