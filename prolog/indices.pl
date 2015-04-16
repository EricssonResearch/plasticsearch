:- module(indices, [
    create/4,
    create/5,
    delete/3,
    delete/4,
    get/4,
    get/5
]).

:- use_module(transport).
:- use_module(util).

create(Ps, Index, Body, Reply) :-
    create(Ps, Index, _{}, Body, Reply).

create(Ps, Index, Params, Body, Reply) :-
    make_context(Index, Context),
    perform_request(Ps, post, Context, Params, Body, _, Reply).

delete(Ps, Index, Reply) :-
    delete(Ps, Index, _{}, Reply).

delete(Ps, Index, Params, Reply) :-
    make_context(Index, Context),
    perform_request(Ps, delete, Context, Params, _, Reply).

get(Ps, Index, Feature, Reply) :-
    get(Ps, Index, Feature, _{}, Reply).

get(Ps, Index, Feature, Params, Reply) :-
    make_context([Index, Feature], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

analyze(Ps, Index, Body, Reply) :-
    analyze(Ps, Index, _{}, Body, Reply).

analyze(Ps, Index, Params, Body, Reply) :-
    make_context([Index, '_analyze'], Context),
    (   var(Body)
    ->  perform_request(Ps, get, Context, Params, _, Reply)
    ;   perform_request(Ps, post, Context, Params, Body, _, Reply)
    ).