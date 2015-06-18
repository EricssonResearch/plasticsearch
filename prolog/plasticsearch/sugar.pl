:- module(sugar, [
    '.'/3       % +Ps, +Term, -Result
]).

/** <module> syntactic sugar
It's all about "." ;)

@author Hongxin Liang
@license Apache License Version 2.0
*/

%% '.'(+Ps, +Term, -Result) is semidet.
%
% Syntactic sugar for invoking APIs.

'.'(Ps, cluster, [cluster, Ps]) :- !.
'.'(Ps, nodes, [nodes, Ps]) :- !.
'.'(Ps, indices, [indices, Ps]) :- !.
'.'(Ps, snapshots, [snapshots, Ps]) :- !.

'.'([Module, Ps], Term, true) :- !,
    Term =.. [Name|Args],
    TermWithPs =.. [Name|[Ps|Args]],
    ModuledTerm =.. [:, Module, TermWithPs],
    call(ModuledTerm).

'.'(Ps, Term, true) :-
    Term =.. [Name|Args],
    TermWithPs =.. [Name|[Ps|Args]],
    ModuledTerm =.. [:, plasticsearch, TermWithPs],
    call(ModuledTerm).
