:- module(registry, [
    safe_recorda/2,
    safe_erase/1,
    safe_recorded/2,
    safe_update/2
]).

:- initialization(mutex_create(_, [alias(plasticsearch)]), restore).

safe_recorda(Key, Term) :-
    with_mutex(plasticsearch, recorda(Key, Term)).

safe_erase(Key) :-
    with_mutex(plasticsearch, (
        recorded(Key, _, Ref),
        erase(Ref))
    ).

safe_recorded(Key, Term) :-
    with_mutex(plasticsearch, recorded(Key, Term)).


safe_update(Key, Term) :-
    with_mutex(plasticsearch, (
        recorded(Key, _, Ref),
        erase(Ref),
        recorda(Key, Term))
    ).
