:- module(ex1, []).

:- use_module(library(plasticsearch)).

:- debug(ex1).

index_op :-
    plasticsearch(Ps, ['http://192.121.150.101:8200', 'http://192.121.150.101:9200'],
        [dead_timeout(1), retry_on_status([502, 503, 504])]),
    debug(ex1, 'Plasticsearch ~w', Ps),
    catch(Ps.indices.create(es_test, _{refresh:true},
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
        PutMappingReply1), _, true),
    debug(ex1, 'PutMapping1 ~w', PutMappingReply1),
    catch(Ps.indices.put_mapping(es_test, retweet,
        _{retweet:_{properties:_{message:_{type:string, store:true}}}},
        PutMappingReply2), _, true),
    debug(ex1, 'PutMapping2 ~w', PutMappingReply2),
    catch(Ps.indices.get_mapping(es_test, tweet, GetMappingReply1), _, true),
    debug(ex1, 'GetMapping1 ~w', GetMappingReply1),
    catch(Ps.indices.get_mapping(es_test, '', GetMappingReply2), _, true),
    debug(ex1, 'GetMapping2 ~w', GetMappingReply2),
    catch(Ps.indices.get_field_mapping(es_test, tweet, message, GetFieldMappingReply), _, true),
    debug(ex1, 'GetFieldMappingReply ~w', GetFieldMappingReply),
    catch(Ps.indices.delete_mapping(es_test, tweet, DeleteMappingReply1), _, true),
    debug(ex1, 'DeleteMappingReply1 ~w', DeleteMappingReply1),
    catch(Ps.indices.delete_mapping(es_test, retweet, DeleteMappingReply2), _, true),
    debug(ex1, 'DeleteMappingReply2 ~w', DeleteMappingReply2),
    catch(Ps.indices.put_alias(es_test, es_test_alias, '', PutAliasReply), _, true),
    debug(ex1, 'PutAliasReply ~w', PutAliasReply),
    Ps.indices.exists_alias(es_test, es_test_alias),
    \+ Ps.indices.exists_alias(es_test, some_alias),
    catch(Ps.indices.get_alias(es_test, es_test_alias, GetAliasReply), _, true),
    debug(ex1, 'GetAliasReply ~w', GetAliasReply),
    catch(Ps.indices.get_aliases(es_test, 'es_test_alias,some_alias', GetAliasesReply), _, true),
    debug(ex1, 'GetAliasesReply ~w', GetAliasesReply),
    catch(Ps.indices.update_aliases(_{actions:[_{add:_{index:es_test, alias:es_test_alias1}}]},
        UpdateAliasesReply), _, true),
    debug(ex1, 'UpdateAliasesReply ~w', UpdateAliasesReply),
    Ps.indices.exists_alias(es_test, es_test_alias1),
    catch(Ps.indices.delete_alias(es_test, es_test_alias1, DeleteAliasReply), _, true),
    debug(ex1, 'DeleteAliasReply ~w', DeleteAliasReply),
    \+ Ps.indices.exists_alias(es_test, es_test_alias1),
    catch(Ps.indices.put_template(template1,
        _{
            template:'te*',
            settings:_{number_of_shards:1},
            mappings:_{type1:_{'_source':_{enabled:false}}}
        },
        PutTemplateReply), _, true),
    debug(ex1, 'PutTemplateReply ~w', PutTemplateReply),
    Ps.indices.exists_template(template1),
    catch(Ps.indices.get_template(template1, GetTemplateReply), _, true),
    debug(ex1, 'GetTemplateReply ~w', GetTemplateReply),
    catch(Ps.indices.delete_template(template1, DeleteTemplateReply), _, true),
    debug(ex1, 'DeleteTemplateReply ~w', DeleteTemplateReply),
    \+ Ps.indices.exists_template(template1),
    catch(Ps.indices.put_settings(es_test,
        _{
            index:_{number_of_replicas:10,refresh_interval:10}
        },
        PutSettingsReply), _, true),
    debug(ex1, 'PutSettingsReply ~w', PutSettingsReply),
    catch(Ps.indices.get_settings(es_test, 'index.number_of_replicas', GetSettingsReply1), _, true),
    debug(ex1, 'GetSettingsReply1 ~w', GetSettingsReply1),
    catch(Ps.indices.get_settings(es_test, '', GetSettingsReply2), _, true),
    debug(ex1, 'GetSettingsReply2 ~w', GetSettingsReply2),
    catch(Ps.indices.put_warmer(es_test, tweet, warmer1,
        _{
            query:_{match_all:_{}},
            aggs:_{aggs_1:_{terms:_{field:field}}}
        },
        PutWarmerReply), _, true),
    debug(ex1, 'PutWarmerReply ~w', PutWarmerReply),
    catch(Ps.indices.get_warmer(es_test, tweet, warmer1, GetWarmerReply1), _, true),
    debug(ex1, 'GetWarmerReply1 ~w', GetWarmerReply1),
    catch(Ps.indices.delete_warmer(es_test, warmer1, DeleteWarmerReply), _, true),
    debug(ex1, 'DeleteWarmerReply ~w', DeleteWarmerReply),
    catch(Ps.indices.get_warmer(es_test, '', '', GetWarmerReply2), _, true),
    debug(ex1, 'GetWarmerReply2 ~w', GetWarmerReply2),
    catch(Ps.indices.status(es_test, StatusReply), _, true),
    debug(ex1, 'StatusReply ~w', StatusReply),
    catch(Ps.indices.stats(es_test, '', StatsReply), _, true),
    debug(ex1, 'StatsReply ~w', StatsReply),
    catch(Ps.indices.segments(es_test, SegmentsReply), _, true),
    debug(ex1, 'SegmentsReply ~w', SegmentsReply),
    catch(Ps.indices.optimize(es_test, OptimizeReply), _, true),
    debug(ex1, 'OptimizeReply ~w', OptimizeReply),
    catch(Ps.indices.validate_query(es_test, tweet, _{q:'user:foo'}, '', ValidateQueryReply1), _, true),
    debug(ex1, 'ValidateQueryReply1 ~w', ValidateQueryReply1),
    catch(Ps.indices.validate_query(es_test, tweet,
        _{
            query:_{
                filtered:_{
                    query:_{
                        query_string:_{
                            query:'*:*'
                        }
                    },
                    filter:_{
                        term:_{user:kimchy}
                    }
                }
            }
        }, ValidateQueryReply2), _, true),
    debug(ex1, 'ValidateQueryReply2 ~w', ValidateQueryReply2),
    catch(Ps.indices.clear_cache(es_test, ClearCacheReply), _, true),
    debug(ex1, 'ClearCacheReply ~w', ClearCacheReply),
    catch(Ps.indices.recovery(es_test, RecoveryReply), _, true),
    debug(ex1, 'RecoveryReply ~w', RecoveryReply),
    % catch(Ps.indices.snapshot_index(es_test, SnapshotReply), _, true),
    % debug(ex1, 'SnapshotReply ~w', SnapshotReply),
    catch(Ps.indices.upgrade(es_test, UpgradeReply), _, true),
    debug(ex1, 'UpgradeReply ~w', UpgradeReply),
    catch(Ps.indices.get_upgrade(es_test, GetUpgradeReply), _, true),
    debug(ex1, 'GetUpgradeReply ~w', GetUpgradeReply),
    catch(Ps.indices.delete(es_test, DeleteReply), _, true),
    debug(ex1, 'Delete ~w', DeleteReply),
    destroy(Ps).

cluster_op :-
    plasticsearch(Ps, ['http://192.121.150.101:8200', 'http://192.121.150.101:9200'],
        [dead_timeout(1), retry_on_status([502, 503, 504])]),
    debug(ex1, 'Plasticsearch ~w', Ps),
    catch(Ps.indices.create(es_test,
        _{settings: _{index: _{'mapping.allow_type_wrapper': true}}},
        CreateReply), _, true),
    debug(ex1, 'Create ~w', CreateReply),
    catch(Ps.cluster.health(es_test, HealthReply1), _, true),
    debug(ex1, 'HealthReply1 ~w', HealthReply1),
    catch(Ps.cluster.health('', HealthReply2), _, true),
    debug(ex1, 'HealthReply2 ~w', HealthReply2),
    catch(Ps.cluster.pending_tasks(PendingTasksReply), _, true),
    debug(ex1, 'PendingTasksReply ~w', PendingTasksReply),
    catch(Ps.cluster.state(es_test, '', StateReply), _, true),
    debug(ex1, 'StateReply ~w', StateReply),
    catch(Ps.cluster.stats('', StatsReply), _, true),
    debug(ex1, 'StatsReply ~w', StatsReply),
    catch(Ps.cluster.reroute(_{
            commands:[
                _{
                    move:_{index:es_test, shard:0, from_node:node1, to_node:node2}
                },
                _{
                    allocate:_{index:es_test, shard:1, node:node3}
                }
            ]
        }, _), error(plasticsearch_exception(400, _)), true),
    catch(Ps.indices.delete(es_test, DeleteReply), _, true),
    catch(Ps.cluster.put_settings(_{persistent:_{'discovery.zen.minimum_master_nodes':1}},
        PutSettingsReply), _, true),
    debug(ex1, 'PutSettingsReply ~w', PutSettingsReply),
    catch(Ps.cluster.get_settings(GetSettingsReply), _, true),
    debug(ex1, 'GetSettingsReply ~w', GetSettingsReply),
    catch(Ps.indices.delete(es_test, DeleteReply), _, true),
    debug(ex1, 'Delete ~w', DeleteReply),
    destroy(Ps).

nodes_op :-
    plasticsearch(Ps, ['http://192.121.150.101:8200', 'http://192.121.150.101:9200'],
        [dead_timeout(1), retry_on_status([502, 503, 504])]),
    debug(ex1, 'Plasticsearch ~w', [Ps]),
    catch(Ps.nodes.info('', 'jvm,process', InfoReply), _, true),
    debug(ex1, 'InfoReply ~w', InfoReply),
    catch(Ps.nodes.shutdown('node1', ShutdownReply), _, true),
    debug(ex1, 'ShutdownReply ~w', ShutdownReply),
    catch(Ps.nodes.stats('', '', '', StatsReply), _, true),
    debug(ex1, 'StatsReply ~w', StatsReply),
    catch(Ps.nodes.hot_threads('', HotThreadsReply), _, true),
    debug(ex1, 'HotThreadsReply ~w', HotThreadsReply),
    destroy(Ps).

ps_op :-
    plasticsearch(Ps, ['http://192.121.150.101:8200', 'http://192.121.150.101:9200'],
        [dead_timeout(1), retry_on_status([502, 503, 504])]),
    Ps.ping,
    catch(Ps.info(InfoReply), _, true),
    debug(ex1, 'InfoReply ~w', InfoReply),
    catch(Ps.indices.create(es_test,
        _{settings: _{index: _{'mapping.allow_type_wrapper': true}}},
        CreateReply), _, true),
    debug(ex1, 'Create ~w', CreateReply),
    get_current_time_as_atom(Time1),
    catch(Ps.create(es_test, tweet, '', _{
        tweet:_{user:kimchy, post_date:Time1, message:'trying out Elasticsearch'}
    }, CreateIndexReply1), _, true),
    debug(ex1, 'CreateIndexReply1 ~w', CreateIndexReply1),
    catch(Ps.create(es_test, tweet, '1', _{
        tweet:_{user:kimchy, post_date:Time1, message:'trying out Elasticsearch'}
    }, CreateIndexReply2), _, true),
    debug(ex1, 'CreateIndexReply2 ~w', CreateIndexReply2),
    catch(Ps.create(es_test, tweet, '1', _{
        tweet:_{user:kimchy, post_date:Time1, message:'trying out Elasticsearch'}
    }, CreateIndexReply3), _, true),
    debug(ex1, 'CreateIndexReply3 ~w', CreateIndexReply3),
    catch(Ps.index(es_test, tweet, '', _{
        tweet:_{user:kimchy, post_date:Time1, message:'trying out Elasticsearch'}
    }, IndexReply1), _, true),
    debug(ex1, 'IndexReply1 ~w', IndexReply1),
    catch(Ps.index(es_test, tweet, '2', _{
        tweet:_{user:kimchy, post_date:Time1, message:'trying out Elasticsearch'}
    }, IndexReply2), _, true),
    debug(ex1, 'IndexReply2 ~w', IndexReply2),
    Ps.exists(es_test, '_all', '1'),
    Ps.exists(es_test, tweet, '1'),
    \+ Ps.exists(es_test, tweet, '3'),
    catch(Ps.get(es_test, tweet, '2', GetReply1), _, true),
    debug(ex1, 'GetReply1 ~w', GetReply1),
    catch(Ps.get(es_test, '_all', '2', GetReply2), _, true),
    debug(ex1, 'GetReply2 ~w', GetReply2),
    catch(Ps.get_source(es_test, tweet, '2', GetSourceReply), _, true),
    debug(ex1, 'GetSourceReply ~w', GetSourceReply),
    catch(Ps.mget(es_test, '', _{
        docs:[_{'_index':es_test, '_type':tweet, '_id':'2'},
              _{'_index':es_test, '_type':tweet, '_id':'1'}]
    }, MGetReply), _, true),
    debug(ex1, 'MGetReply ~w', MGetReply),
    catch(Ps.update(es_test, tweet, '2', _{refresh:true}, _{
        doc:_{new_field:new_value}
    }, UpdateIndexReply), _, true),
    debug(ex1, 'UpdateIndexReply ~w', UpdateIndexReply),
    catch(Ps.search(es_test, tweet, _{q:'user:kimchy', scroll:'2'}, _, SearchReply1), _, true),
    debug(ex1, 'SearchReply1 ~w', SearchReply1),
    catch(Ps.search(es_test, tweet, _{
        query:_{term:_{user:kimchy}}
    }, SearchReply2), _, true),
    debug(ex1, 'SearchReply2 ~w', SearchReply2),
    catch(Ps.search_shards(es_test, tweet, SearchShardsReply), _, true),
    debug(ex1, 'SearchShardsReply ~w', SearchShardsReply),
    catch(Ps.search_template('', '', _, SearchTemplateReply1), _, true),
    debug(ex1, 'SearchTemplateReply1 ~w', SearchTemplateReply1),
    catch(Ps.explain(es_test, tweet, '2', _{
        query:_{term:_{user:kimchy}}
    }, ExplainReply), _, true),
    debug(ex1, 'ExplainReply ~w', ExplainReply),
    get_dict('_scroll_id', SearchReply1, ScrollID),
    debug(ex1, 'ScrollID ~w', ScrollID),
    catch(Ps.scroll(ScrollID, ScrollReply), _, true),
    debug(ex1, 'ScrollReply ~w', ScrollReply),
    catch(Ps.clear_scroll(ScrollID, _, ClearScrollReply1), _, true),
    debug(ex1, 'ClearScrollReply1 ~w', ClearScrollReply1),
    catch(Ps.clear_scroll('', _, ClearScrollReply2), _, true),
    debug(ex1, 'ClearScrollReply2 ~w', ClearScrollReply2),
    catch(Ps.delete(es_test, tweet, '2', DeleteDocReply), _, true),
    debug(ex1, 'DeleteDocReply ~w', DeleteDocReply),
    catch(Ps.count(es_test, tweet, _{q:'user:kimchy'}, _, CountReply), _, true),
    debug(ex1, 'CountReply ~w', CountReply),
    catch(Ps.bulk('', '', _{refresh:true}, [
        _{update:_{'_index':es_test, '_type':tweet, '_id':'1'}},
        _{doc:_{message:'it works!'}},
        _{update:_{'_index':es_test, '_type':tweet, '_id':'2'}},
        _{doc:_{message:'it works!'}}
    ], BulkReply), _, true),
    debug(ex1, 'BulkReply ~w', BulkReply),
    catch(Ps.msearch('', '', [
        _{index:es_test},
        _{query:_{match_all:_{}}, from:0, size:10},
        _{index:es_test, search_type:count},
        _{query:_{match_all:_{}}}
    ], MSearchReply), _, true),
    debug(ex1, 'MSearchReply ~w', MSearchReply),
    catch(Ps.delete_by_query(es_test, tweet,
        _{query:_{term:_{user:kimchy}}},
        DeleteByQueryReply), _, true),
    debug(ex1, 'DeleteByQueryReply ~w', DeleteByQueryReply),
    catch(Ps.suggest(es_test,
        _{'my-suggestion':_{text:'the amsterdma meetpu', term:_{field:body}}},
        SuggestReply), _, true),
    debug(ex1, 'SuggestReply ~w', SuggestReply),
    catch(Ps.indices.delete(es_test, DeleteReply), _, true),
    debug(ex1, 'Delete ~w', DeleteReply),
    destroy(Ps).

get_current_time_as_atom(Time) :-
    get_time(T),
    format_time(atom(Time), '%FT%T%z', T, posix).