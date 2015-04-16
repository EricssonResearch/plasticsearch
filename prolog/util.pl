:- module(util, [
    make_context/2,     % +Parts, -Context
    random/2,           % +List, -Elem
    non_empty_index/1,  % +Index
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
    atomic_list_concat(Parts1, /, Context0),
    Parts1 = [H|_],
    (   H \= ''
    -> atom_concat(/, Context0, Context)
    ;  Context = Context0
    ).

%% random(+List, -Elem) is det.
%
% Randomly select one element from a list.

random(List, Elem) :-
    length(List, Length),
    random_between(0, Length, Index),
    nth0(Index, List, Elem).

%% non_empty_index(+Index) is det.
%
% Throw an exception if = Index = is empty atom
% or empty list.

non_empty_index(Index) :-
    (Index = ''; Index = []), !,
    throw(error(plasticsearch_exception(na, 'Empty value passed for a required argument \'index\'.'))).

non_empty_index(_).

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
