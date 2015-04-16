:- module(ex1, []).

:- use_module(library(plasticsearch)).

:- debug(ex1).

index_op :-
    plasticsearch(Ps, ['http://192.121.150.101:8200', 'http://192.121.150.101:9200'],
        [dead_timeout(1), retry_on_status([502, 503, 504])]),
    debug(ex1, 'Plasticsearch ~w', [Ps]),
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
    catch(Ps.indices.flush(es_test, FlushReply), _, true),
    debug(ex1, 'Flush ~w', FlushReply),
    catch(Ps.indices.close_index(es_test, CloseReply), _, true),
    debug(ex1, 'Close ~w', CloseReply),
    catch(Ps.indices.open_index(es_test, OpenReply), _, true),
    debug(ex1, 'Open ~w', OpenReply),
    Ps.indices.exists(es_test),
    catch(Ps.indices.put_mapping(es_test, tweet,
        _{tweet:_{properties:_{message:_{type:string, store:true}}}},
        PutMappingReply), _, true),
    debug(ex1, 'PutMapping ~w', PutMappingReply),
    catch(Ps.indices.delete(es_test, DeleteReply), _, true),
    debug(ex1, 'Delete ~w', DeleteReply),
    destroy(Ps).
