:- module(util, [
    make_context/2,     % +Parts, -Context
    non_empty/2,        % +Input, +Name
    non_empty/3,        % +Input, +Name, +ThrowIfEmpty
    extract_param/5     % +Params, -NewParams, +Name, -Value, +DefaultValue
]).

/** <module> Utilities

@auther Hongxin Liang
@license TBD
*/

:- use_module(library(lists)).

%% make_context(+Parts, -Context) is det.
%
% Create an HTTP context.

make_context(Parts, Context) :-
    (   is_list(Parts)
    ->  Parts1 = Parts
    ;   Parts1 = [Parts]
    ),
    remove_empty_atom(Parts1, Parts2),
    atomic_list_concat([''|Parts2], /, Context).

remove_empty_atom([], []) :- !.
remove_empty_atom([H|T], L) :-
    H = '', !,
    remove_empty_atom(T, L).
remove_empty_atom([H|T], L) :-
    remove_empty_atom(T, L0),
    L = [H|L0].

%% non_empty(+Input, +Name) is det.
%% non_empty(+Input, +Name, +ThrowIfEmpty) is det.
%
% Throw an exception if = Input = is empty atom
% or empty list and = ThrowIfEmpty = is = true =.

non_empty(Input, Name) :-
    non_empty(Input, Name, true).

non_empty(Input, Name, ThrowIfEmpty) :-
    once((Input = ''; Input = [])), !,
    (   ThrowIfEmpty
    ->  atomic_list_concat(['Empty value passed for a required argument \'',
            Name, '\'.'], Message),
        throw(error(plasticsearch_exception(na, Message)))
    ).

non_empty(_, _, _).

%% extract_param(+Params, -NewParams, +Name, -Value, +DefaultValue) is det.
%
% Extract parameter from dictionary and return = DefaultValue = is
% the specified parameter does not exist.

extract_param(Params, NewParams, Name, Value, DefaultValue) :-
    (   del_dict(Name, Params, Value, NewParams)
    ->  true
    ;   Value = DefaultValue,
        NewParams = Params
    ).
