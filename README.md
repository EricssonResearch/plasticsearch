# plasticsearch

Plasticsearch is an Elasticsearch Client implemented in Prolog.

The implementation is based on Python Elasticsearch Client.
Architecture, design, and modularization are kept the same as
Python Elasticsearch Client in order to make it easier to use
for developers who are familiar with Python Elasticsearch Client.

## Installation

Using SWI-Prolog 7 or later.

    ?- pack_install('http://git.cf.ericsson.net/ehonlia/plasticsearch.git').

Source code available and pull requests accepted
[here](http://git.cf.ericsson.net/ehonlia/plasticsearch).

@author Hongxin Liang <hongxin.liang@ericsson.com>

@license Apache License Version 2.0

## Examples

To create an index and add a document:

    :- use_module(library(plasticsearch/sugar)).
    :- use_module(library(plasticsearch/plasticsearch)).

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

## License

Copyright 2015 Ericsson

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
