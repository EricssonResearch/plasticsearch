:- module(util, [
    make_context/2
]).

make_context(Parts, Context) :-
    (   is_list(Parts)
    ->  Parts1 = Parts
    ;   Parts1 = [Parts]
    ),
    atomic_list_concat(Parts1, /, Context0),
    atom_concat(/, Context0, Context).
