# plasticsearch


## Installation

Using SWI-Prolog 7 or later.

    ?- pack_install('http://git.cf.ericsson.net/ehonlia/plasticsearch.git').

Source code available and pull requests accepted
[here](http://git.cf.ericsson.net/ehonlia/plasticsearch).

@author Hongxin Liang <hongxin.liang@ericsson.com>

@license TBD

## Examples

To create an index and add a document:

    :- use_module(library(sugar)).
    :- use_module(library(plasticsearch)).

    create :-
        plasticsearch(Ps, ['http://192.121.150.101:8200', 'http://192.121.150.101:9200'],
            [dead_timeout(10)]),
        Ps.indices.create(es_test, _{refresh:true},
            _{settings: _{index: _{'mapping.allow_type_wrapper': true}}},
            CreateReply),
        get_current_time_as_atom(Time1),
        Ps.create(es_test, tweet, '', _{refresh:true}, _{
                tweet:_{user:kimchy1, post_date:Time1, message:'trying out Elasticsearch'}
            }, CreateIndexReply1),
        Ps.indices.delete(es_test, DeleteReply),
        destroy(Ps).

For more examples, please check source code under `examples` directory.
