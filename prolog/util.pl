:- module(util, [
    make_context/2,
    random/2
]).

:- use_module(library(lists)).

make_context(Parts, Context) :-
    (   is_list(Parts)
    ->  Parts1 = Parts
    ;   Parts1 = [Parts]
    ),
    atomic_list_concat(Parts1, /, Context0),
    Parts1 = [H|_],
    (   H \= ''
    -> atom_concat(/, Context0, Context)
    ;  Context = Context0
    ).

random(List, Elem) :-
    length(List, Length),
    random_between(0, Length, Index),
    nth0(Index, List, Elem).
