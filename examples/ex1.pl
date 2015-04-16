:- module(ex1, []).

:- use_module(library(plasticsearch)).

:- debug(ex1).

index_op :-
    plasticsearch(Ps, ['http://honnix-ws:9200'],
        [dead_timeout(1), retry_on_status([502, 503, 504])]),
    catch(Ps.indices.create(es_test,
        _{settings: _{index: _{'mapping.allow_type_wrapper': true}}},
        CreateReply), _, true),
    debug(ex1, 'Create ~w', CreateReply),
    catch(Ps.indices.get(es_test, '', IndexInfo1), _, true),
    debug(ex1, 'IndexInfo1 ~w', IndexInfo1),
    catch(Ps.indices.get(es_test, '_settings,_mappings', IndexInfo2), _, true),
    debug(ex1, 'IndexInfo2 ~w', IndexInfo2),
    sleep(1), % No shard available for [org.elasticsearch.action.admin.indices.analyze.AnalyzeRequest@ca11c28]
    catch(Ps.indices.analyze(es_test, 'this is a test', AnalyzeReply1), _, true),
    debug(ex1, 'Analyze1 ~w', AnalyzeReply1),
    catch(Ps.indices.analyze(es_test, _{analyzer:standard, text:'this is a test'}, _, AnalyzeReply2), _, true),
    debug(ex1, 'Analyze2 ~w', AnalyzeReply2),
    catch(Ps.indices.analyze(es_test, _{analyzer:standard}, 'this is a test', AnalyzeReply3), _, true),
    debug(ex1, 'Analyze3 ~w', AnalyzeReply3),
    catch(Ps.indices.analyze('', _{analyzer:standard}, 'this is a test', AnalyzeReply4), _, true),
    debug(ex1, 'Analyze4 ~w', AnalyzeReply4),
    catch(Ps.indices.refresh(es_test, RefreshReply), _, true),
    debug(ex1, 'Refresh ~w', RefreshReply),
    catch(Ps.indices.delete(es_test, DeleteReply), _, true),
    debug(ex1, 'Delete ~w', DeleteReply),
    destroy(Ps).
