:- module(transport, [
    perform_request/6,      % +Ps, +HTTPMethod, +Context, +Params, -Status, -Reply
    perform_request/7       % +Ps, +HTTPMethod, +Context, +Params, +Body, -Status, -Reply
]).

/** <module> Transport related logic.

@auther Hongxin Liang
@license TBD
*/

:- use_module(library(uri)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

:- use_module(connection_pool).
:- use_module(registry).
:- use_module(util).

%% perform_request(+Ps, +HTTPMethod, +Context, +Params, -Status, -Reply) is semidet.
%% perform_request(+Ps, +HTTPMethod, +Context, +Params, +Body, -Status, -Reply) is semidet.
%
% Perform actual HTTP request. For GET and DELETE methods, body is not supported.

perform_request(Ps, get, Context, Params, Status, Reply) :- !,
    http_operation_with_retry(Ps, Context, Params, http_get, Status, Reply).

perform_request(Ps, delete, Context, Params, Status, Reply) :- !,
    http_operation_with_retry(Ps, Context, Params, http_delete, Status, Reply).

perform_request(Ps, head, Context, Params, Status, Reply) :- !,
    http_operation_with_retry(Ps, Context, Params, http_head, Status, Reply).

perform_request(Ps, post, Context, Params, Body, Status, Reply) :- !,
    wrap_body(Body, WrappedBody),
    http_operation_with_retry(Ps, Context, Params, http_post(WrappedBody), Status, Reply).

perform_request(Ps, put, Context, Params, Body, Status, Reply) :-
    wrap_body(Body, WrappedBody),
    http_operation_with_retry(Ps, Context, Params, http_put(WrappedBody), Status, Reply).

http_head(URL, _, Options) :-
    uri_components(URL, uri_components(Scheme, Authority, Path, Search, Fragment)),
    uri_authority_components(Authority, uri_authority(User, Password, Host, Port)),
    uri_authority_components(Authority1, uri_authority(_, _, Host, Port)),
    uri_components(URL1, uri_components(Scheme, Authority1, Path, Search, Fragment)),
    http_open(URL1, Stream, [method(head),authorization(basic(User, Password))|Options]),
    close(Stream).
    
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
    extract_param(Params, NewParams, timeout, Timeout, infinite),
    extract_param(NewParams, NewParams1, ignore, Ignore, []),
    http_operation_with_retry0(Ps, Context, NewParams1, Operation, RetryOnStatus, RetryOnTimeout, MaxRetries, Timeout, Ignore, Status, Reply).

http_operation_with_retry0(Ps, Context, Params, Operation, RetryOnStatus, RetryOnTimeout, MaxRetries, Timeout, Ignore, Status, Reply) :-
    get_connection(Ps, Connection),
    compose_url(Connection, Context, Params, URL),
    Operation =.. [Name|Args],
    Operation1 =.. [Name|[URL|Args]],
    (   catch(call(Operation1, Reply0, [status_code(Status0), timeout(Timeout), json_object(dict)]), E, true)
    ->  (   var(E)
        ->  handle_status(Status0, Reply0, RetryOnStatus, Ignore, Success, Retry)
        ;   handle_exception(E, RetryOnTimeout, Success, Retry)
        )
    ),
    (   \+ Success
    ->  (   Retry
        ->  mark_dead(Ps, Connection),
            (   MaxRetries > 0
            ->  debug(transport, 'retrying... count ~w', [MaxRetries]),
                http_operation_with_retry0(Ps, Context, Params, Operation, RetryOnStatus,
                    RetryOnTimeout, MaxRetries - 1, Timeout, Ignore, Status, Reply)
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

compose_url(uri_components(Scheme, Authority, Path, Search, Fragment), Context, Params, URL) :-
    atom_concat(Path, Context, NewPath),
    dict_pairs(Params, _, Pairs),
    uri_query_components(Search, Pairs),
    uri_components(URL, uri_components(Scheme, Authority, NewPath, Search, Fragment)).

handle_status(Status, Reply, RetryOnStatus, Ignore, Success, Retry) :-
    (   once((Status >= 200, Status < 300; memberchk(Status, Ignore)))
    ->  Success = true
    ;   debug(transport, 'status code ~w, reply ~w', [Status, Reply]),
        match_status_and_throw_immediately(Status, Reply),
        (   memberchk(Status, RetryOnStatus)
        ->  Retry = true
        ;   Retry = false
        ),
        Success = false
    ).

match_status_and_throw_immediately(Status, Reply) :-
    (   memberchk(Status, [400, 401, 403, 404, 409])
    ->  throw(error(plasticsearch_exception(Status, Reply)))
    ;   true
    ).

handle_exception(E, RetryOnTimeout, false, Retry) :-
    (   E = error(socket_error(_), _)
    ->  Retry = true
    ;   (   E = error(timeout_error(_, _) ,_)
        ->  Retry = RetryOnTimeout
        ;   Retry = false
        )
    ).
