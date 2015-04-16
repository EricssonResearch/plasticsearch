:- module(transport, [
    perform_request/6,
    perform_request/7
]).

:- use_module(library(uri)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

:- use_module(connection_pool).
:- use_module(registry).

perform_request(Ps, get, Context, Params, Status, Reply) :- !,
    http_operation_with_retry(Ps, Context, Params, http_get, Status, Reply).

perform_request(Ps, delete, Context, Params, Status, Reply) :-
    http_operation_with_retry(Ps, Context, Params, http_delete, Status, Reply).

perform_request(Ps, post, Context, Params, Body, Status, Reply) :- !,
    wrap_body(Body, WrappedBody),
    http_operation_with_retry(Ps, Context, Params, http_post(WrappedBody), Status, Reply).

perform_request(Ps, put, Context, Params, Body, Status, Reply) :-
    wrap_body(Body, WrappedBody),
    http_operation_with_retry(Ps, Context, Params, http_put(WrappedBody), Status, Reply).

wrap_body(Body, WrappedBody) :-
    is_dict(Body), !,
    WrappedBody = json(Body).

wrap_body(Body, WrappedBody) :-
    atom(Body), !,
    WrappedBody = codes(Body).

http_operation_with_retry(Ps, Context, Params, Operation, Status, Reply) :-
    options(Ps, Options),
    memberchk(retry_on_status(RetryOnStatus), Options),
    memberchk(retry_on_timeout(RetryOnTimeout), Options),
    memberchk(max_retries(MaxRetries), Options),
    http_operation_with_retry0(Ps, Context, Params, Operation, RetryOnStatus, RetryOnTimeout, MaxRetries, Status, Reply).

http_operation_with_retry0(Ps, Context, Params, Operation, RetryOnStatus, RetryOnTimeout, MaxRetries, Status, Reply) :-
    get_timeout_option(Params, Timeout, Params1),
    get_connection(Ps, Connection),
    compose_url(Connection, Context, Params1, URL),
    Operation =.. [Name|Args],
    Operation1 =.. [Name|[URL|Args]],
    (   catch(call(Operation1, Reply0, [status_code(Status0), timeout(Timeout), json_object(dict)]), E, true)
    ->  (   var(E)
        ->  (   Status0 >= 200, Status0 < 300
            ->  Success = true
            ;   debug(transport, 'status code ~w, reply ~w', [Status0, Reply0]),
                (   memberchk(Status0, RetryOnStatus)
                ->  Retry = true
                ;   Retry = false
                ),
                Success = false
            )
        ;   (   E = error(socket_error(_), _)
            ->  Retry = true
            ;   (   E = error(timeout_error(_, _) ,_)
                ->  Retry = RetryOnTimeout
                ;   Retry = false
                )
            ),
            Success = false
        )
    ),
    (   \+ Success
    ->  (   Retry
        ->  mark_dead(Ps, Connection),
            (   MaxRetries > 0
            ->  debug(transport, 'retrying... count ~w', [MaxRetries]),
                http_operation_with_retry0(Ps, Context, Params, Operation, RetryOnStatus,
                    RetryOnTimeout, MaxRetries - 1, Status, Reply)
            ;   throw(error(plasticsearch_exception(Status0, Reply0)))
            )
        ;   throw(error(plasticsearch_exception(Status0, Reply0)))
        )
    ;   mark_alive(Ps, Connection),
        Status = Status0,
        Reply = Reply0
    ).

get_timeout_option(Params, Timeout, Params1) :-
    (   del_dict(timeout, Params, Timeout, Params1)
    ->  true
    ;   Timeout = infinite,
        Params1 = Params
    ).

compose_url(uri_components(Scheme, Authority, Path, Search, _), Context, Params, URL) :-
    atom_concat(Path, Context, NewPath),
    dict_pairs(Params, _, Pairs),
    uri_query_components(Search, Pairs),
    uri_components(URL, uri_components(Scheme, Authority, NewPath, Search, _)).
