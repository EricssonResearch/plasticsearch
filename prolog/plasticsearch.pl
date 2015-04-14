:- module(plasticsearch, [
    plasticsearch/2,
    plasticsearch/3
]).

:- use_module(library(uuid)).
:- use_module(library(uri)).

:- initialization(mutex_create(_, [alias(plasticsearch)]), restore).

plasticsearch(Ps, Options) :-
    uuid(Ps),
    uri_components('http://localhost:9200', NormalizedHost),
    fill_options(Options, FullOptions),
    safe_recorda(Ps, plasticsearch([NormalizedHost], FullOptions)).

plasticsearch(Ps, Hosts, Options) :-
    uuid(Ps),
    normalize_hosts(Hosts, NormalizedHosts),
    fill_options(Options, FullOptions),
    safe_recorda(Ps, plasticsearch(NormalizedHosts, FullOptions)).

normalize_hosts([], []) :- !.

normalize_hosts([H|T], NormalizedHosts) :-
    atom(H), !,
    normalize_hosts(T, NormalizedHosts0),
    (   sub_atom_icasechk(H, _, '://')
    ->  Host = H
    ;   atomic_list_concat(['http://', H, ':9200'], Host)
    ),
    uri_components(Host, NormalizedHost),
    NormalizedHosts = [NormalizedHost|NormalizedHosts0].

normalize_hosts([H|T], NormalizedHosts) :-
    compound_name_arity(H, uri_components, 5), !,
    normalize_hosts(T, NormalizedHosts0),
    NormalizedHosts = [H|NormalizedHosts0].

fill_options(Options, FullOptions) :-
    fill_options0([
            dead_timeout(60),
            retry_on_timeout(false),
            timeout_cutoff(5)
        ], Options, FullOptions).

fill_options0([], OldOptions, OldOptions) :- !.
fill_options0([H|T], OldOptions, NewOptions) :-
    fill_options0(T, OldOptions, NewOptions0),
    H =.. [Name, _],
    ToCheck =.. [Name, _],
    (   memberchk(ToCheck, NewOptions0)
    ->  NewOptions = NewOptions0
    ;   NewOptions = [H|NewOptions0]
    ).

safe_recorda(Key, Term) :-
    with_mutex(plasticsearch, recorda(Key, Term)).

safe_erase(Key) :-
    with_mutex(plasticsearch,
        recorded(Key, _, Ref),
        erase(Ref)
    ).

safe_recorded(Key, Term) :-
    with_mutex(plasticsearch, recorded(Key, Term)).