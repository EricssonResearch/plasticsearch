:- module(ex1, []).

:- use_module(library(plasticsearch)).

:- debug(ex1).

index_op :-
    plasticsearch(Ps, ['http://honnix-ws:80', 'http://honnix-ws:9200'],
        [dead_timeout(1), retry_on_status([502, 503, 504])]),
    catch(Ps.indices.create(es_test,
        _{settings: _{index: _{'mapping.allow_type_wrapper': true}}},
        Reply1), _, true),
    debug(ex1, 'Create ~w', Reply1),
    recorded(_, V1, _),
    debug(ex1, 'Registry ~w', V1),
    sleep(5),
    catch(Ps.indices.get(es_test, '', IndexInfo1), _, true),
    debug(ex1, 'Index info ~w', IndexInfo1),
    recorded(_, V2, _),
    debug(ex1, 'Registry ~w', V2),
    catch(Ps.indices.get(es_test, '_settings,_mappings', IndexInfo2), _, true),
    debug(ex1, 'Index info ~w', IndexInfo2),
    recorded(_, V3, _),
    debug(ex1, 'Registry ~w', V3),
    catch(Ps.indices.delete(es_test, Reply2), _, true),
    debug(ex1, 'Delete ~w', Reply2),
    recorded(_, V4, _),
    debug(ex1, 'Registry ~w', V4),
    destroy(Ps).
