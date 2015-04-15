:- module(ex1, []).

:- use_module(library(plasticsearch)).

:- debug(ex1).

index_op :-
    plasticsearch(Ps, 'http://honnix-ws:80', [retry_on_status([502, 503, 504])]),
    Ps.indices.create(es_test,
        _{settings: _{index: _{'mapping.allow_type_wrapper': true}}},
        Reply1),
    debug(ex1, '~w', Reply1),
    Ps.indices.get(es_test, '', IndexInfo1),
    debug(ex1, '~w', IndexInfo1),
    Ps.indices.get(es_test, '_settings,_mappings', IndexInfo2),
    debug(ex1, '~w', IndexInfo2),
    Ps.indices.delete(es_test, Reply2),
    debug(ex1, '~w', Reply2),
    destroy(Ps).
