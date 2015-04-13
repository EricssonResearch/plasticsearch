:- module(plasticsearch, [
    plasticsearch/2
]).

:- use_module(library(uuid)).

plasticsearch(Ps) :-
    

plasticsearch(Ps, Hosts) :-
    uuid(Ps),
    normalize_hosts(Hosts, NormalizedHosts),
    asserta(plasticsearch(Ps, NormalizedHosts)).

normalize_hosts([], NormalizedHosts)