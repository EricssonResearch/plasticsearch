:- module(indices, [
    analyze/4,      % +Ps, +Index, +Body, -Reply
    analyze/5,      % +Ps, +Index, +Params, +Body, -Reply
    refresh/3,      % +Ps, +Index, -Reply
    refresh/4,      % +Ps, +Index, +Params, -Reply
    flush/3,        % +Ps, +Index, -Reply
    flush/4,        % +Ps, +Index, +Params, -Reply
    create/4,       % +Ps, +Index, +Body, -Reply
    create/5,       % +Ps, +Index, +Params, +Body, -Reply
    get/4,          % +Ps, +Index, +Feature, -Reply
    get/5,          % +Ps, +Index, +Feature, +Params, -Reply
    open_index/3,   % +Ps, +Index, -Reply
    open_index/4,   % +Ps, +Index, +Params, -Reply
    close_index/3,  % +Ps, +Index, -Reply
    close_index/4,  % +Ps, +Index, +Params, -Reply
    delete/3,       % +Ps, +Index, -Reply
    delete/4,       % +Ps, +Index, +Params, -Reply
    exists/2,       % +Ps, +Index
    exists/3,       % +Ps, +Index, +Params
    put_mapping/5,  % +Ps, +Index, +DocType, +Body, -Reply
    put_mapping/6,  % +Ps, +Index, +DocType, +Params, +Body, -Reply
    get_mapping/4,  % +Ps, +Index, +DocType, -Reply
    get_mapping/5   % +Ps, +Index, +DocType, +Params, -Reply
]).

/** <module> Indices APIs
The indices APIs are used to manage individual indices,
index settings, aliases, mappings, index templates and warmers.

@auther Hongxin Liang
@license TBD
@see http://www.elastic.co/guide/en/elasticsearch/reference/current/indices.html
*/

:- use_module(transport).
:- use_module(util).

%% analyze(+Ps, +Index, +Body, -Reply) is semidet.
%% analyze(+Ps, +Index, +Params, +Body, -Reply) is semidet.
%
% Perform the analysis process on a text and return the tokens breakdown of the text.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-analyze.html).

analyze(Ps, Index, Body, Reply) :-
    analyze(Ps, Index, _{}, Body, Reply).

analyze(Ps, Index, Params, Body, Reply) :-
    make_context([Index, '_analyze'], Context),
    (   var(Body)
    ->  perform_request(Ps, get, Context, Params, _, Reply)
    ;   perform_request(Ps, post, Context, Params, Body, _, Reply)
    ).

%% refresh(+Ps, +Index, -Reply) is semidet.
%% refresh(+Ps, +Index, +Params, -Reply) is semidet.
%
% Explicitly refresh one or more index, making all operations performed
% since the last refresh available for search.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-refresh.html).

refresh(Ps, Index, Reply) :-
    refresh(Ps, Index, _{}, Reply).

refresh(Ps, Index, Params, Reply) :-
    make_context([Index, '_refresh'], Context),
    perform_request(Ps, post, Context, Params, '', _, Reply).

%% flush(+Ps, +Index, -Reply) is semidet.
%% flush(+Ps, +Index, +Params, -Reply) is semidet.
%
% Explicitly flush one or more indices.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-flush.html).

flush(Ps, Index, Reply) :-
    flush(Ps, Index, _{}, Reply).

flush(Ps, Index, Params, Reply) :-
    make_context([Index, '_flush'], Context),
    perform_request(Ps, post, Context, Params, '', _, Reply).

%% create(+Ps, +Index, +Body, -Reply) is semidet.
%% create(+Ps, +Index, +Param, +Body, -Reply) is semidet.
%
% Create an index in Elasticsearch.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-create-index.html).

create(Ps, Index, Body, Reply) :-
    create(Ps, Index, _{}, Body, Reply).

create(Ps, Index, Params, Body, Reply) :-
    non_empty(Index, index),
    make_context(Index, Context),
    perform_request(Ps, post, Context, Params, Body, _, Reply).

%% get(+Ps, +Index, +Feature, -Reply) is semidet.
%% get(+Ps, +Index, +Feature, +Params, -Reply) is semidet.
%
% The get index API allows to retrieve information about one or more indexes.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-get-index.html).

get(Ps, Index, Feature, Reply) :-
    get(Ps, Index, Feature, _{}, Reply).

get(Ps, Index, Feature, Params, Reply) :-
    non_empty(Index, index),
    make_context([Index, Feature], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% open_index(+Ps, +Index, -Reply) is semidet.
%% open_index(+Ps, +Index, +Params, -Reply) is semidet.
%
% Open a closed index to make it available for search.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-open-close.html).

open_index(Ps, Index, Reply) :-
    open_index(Ps, Index, _{}, Reply).

open_index(Ps, Index, Params, Reply) :-
    non_empty(Index, index),
    make_context([Index, '_open'], Context),
    perform_request(Ps, post, Context, Params, '', _, Reply).

%% close_index(+Ps, +Index, -Reply) is semidet.
%% close_index(+Ps, +Index, +Params, -Reply) is semidet.
%
% Close an index to remove it's overhead from the cluster. Closed index
% is blocked for read/write operations.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-open-close.html).

close_index(Ps, Index, Reply) :-
    close_index(Ps, Index, _{}, Reply).

close_index(Ps, Index, Params, Reply) :-
    non_empty(Index, index),
    make_context([Index, '_close'], Context),
    perform_request(Ps, post, Context, Params, '', _, Reply).

%% delete(+Ps, +Index, -Reply) is semidet.
%% delete(+Ps, +Index, +Params, -Reply) is semidet.
%
% Delete an index in Elasticsearch.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-delete-index.html).

delete(Ps, Index, Reply) :-
    delete(Ps, Index, _{}, Reply).

delete(Ps, Index, Params, Reply) :-
    non_empty(Index, index),
    make_context(Index, Context),
    perform_request(Ps, delete, Context, Params, _, Reply).

%% exists(+Ps, +Index) is semidet.
%% exists(+Ps, +Index, +Params) is semidet.
%
% Return a boolean indicating whether given index exists.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-exists.html).

exists(Ps, Index) :-
    exists(Ps, Index, _{}).

exists(Ps, Index, Params) :-
    non_empty(Index, index),
    make_context(Index, Context),
    (   catch(perform_request(Ps, head, Context, Params, _, _), E, true)
    ->  (   var(E)
        ->  true
        ;   E = plasticsearch_exception(404, _)
        )
    ).

%% put_mapping(+Ps, +Index, +DocType, +Body, -Reply) is semidet.
%% put_mapping(+Ps, +Index, +DocType, +Params, +Body, -Reply) is semidet.
%
% Register specific mapping definition for a specific type.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-put-mapping.html).

put_mapping(Ps, Index, DocType, Body, Reply) :-
    put_mapping(Ps, Index, DocType, _{}, Body, Reply).

put_mapping(Ps, Index, DocType, Params, Body, Reply) :-
    forall(member(Value-Name, [DocType-doc_type, Body-body]), non_empty(Value, Name)),
    make_context([Index, '_mapping', DocType], Context),
    perform_request(Ps, put, Context, Params, Body, _, Reply).

%% get_mapping(+Ps, +Index, +DocType, -Reply) is semidet.
%% get_mapping(+Ps, +Index, +DocType, +Params, -Reply) is semidet.
%
% Retrieve mapping definition of index or index/type.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-get-mapping.html).

get_mapping(Ps, Index, DocType, Reply) :-
    get_mapping(Ps, Index, DocType, _{}, Reply).

get_mapping(Ps, Index, DocType, Params, Reply) :-
    make_context([Index, '_mapping', DocType], Context),
    perform_request(Ps, get, Context, Params,  _, Reply).