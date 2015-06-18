:- module(indices, [
    analyze/4,              % +Ps, +Index, +Body, -Reply
    analyze/5,              % +Ps, +Index, +Params, +Body, -Reply
    refresh/3,              % +Ps, +Index, -Reply
    refresh/4,              % +Ps, +Index, +Params, -Reply
    flush/3,                % +Ps, +Index, -Reply
    flush/4,                % +Ps, +Index, +Params, -Reply
    create/4,               % +Ps, +Index, +Body, -Reply
    create/5,               % +Ps, +Index, +Params, +Body, -Reply
    get/4,                  % +Ps, +Index, +Feature, -Reply
    get/5,                  % +Ps, +Index, +Feature, +Params, -Reply
    open_index/3,           % +Ps, +Index, -Reply
    open_index/4,           % +Ps, +Index, +Params, -Reply
    close_index/3,          % +Ps, +Index, -Reply
    close_index/4,          % +Ps, +Index, +Params, -Reply
    delete/3,               % +Ps, +Index, -Reply
    delete/4,               % +Ps, +Index, +Params, -Reply
    exists/2,               % +Ps, +Index
    exists/3,               % +Ps, +Index, +Params
    put_mapping/5,          % +Ps, +Index, +DocType, +Body, -Reply
    put_mapping/6,          % +Ps, +Index, +DocType, +Params, +Body, -Reply
    get_mapping/4,          % +Ps, +Index, +DocType, -Reply
    get_mapping/5,          % +Ps, +Index, +DocType, +Params, -Reply
    get_field_mapping/5,    % +Ps, +Index, +DocType, +Field, -Reply
    get_field_mapping/6,    % +Ps, +Index, +DocType, +Field, +Params, -Reply
    delete_mapping/4,       % +Ps, +Index, +DocType, -Reply
    delete_mapping/5,       % +Ps, +Index, +DocType, +Params, -Reply
    put_alias/5,            % +Ps, +Index, +Alias, +Body, -Reply
    put_alias/6,            % +Ps, +Index, +Alias, +Params, +Body, -Reply
    exists_alias/3,         % +Ps, +Index, +Alias
    exists_alias/4,         % +Ps, +Index, +Alias, +Params
    get_alias/4,            % +Ps, +Index, +Alias, -Reply
    get_alias/5,            % +Ps, +Index, +Alias, +Params, -Reply
    get_aliases/4,          % +Ps, +Index, +Aliases, -Reply
    get_aliases/5,          % +Ps, +Index, +Aliases, +Params, -Reply
    update_aliases/3,       % +Ps, +Body, -Reply
    update_aliases/4,       % +Ps, +Params, +Body, -Reply
    delete_alias/4,         % +Ps, +Index, +Alias, -Reply
    delete_alias/5,         % +Ps, +Index, +Alias, +Params, -Reply
    put_template/4,         % +Ps, +Name, +Body, -Reply
    put_template/5,         % +Ps, +Name, +Params, +Body, -Reply
    exists_template/2,      % +Ps, +Name
    exists_template/3,      % +Ps, +Name, +Params
    get_template/3,         % +Ps, +Name, -Reply
    get_template/4,         % +Ps, +Name, +Params, -Reply
    delete_template/3,      % +Ps, +Name, -Reply
    delete_template/4,      % +Ps, +Name, +Params, -Reply
    get_settings/4,         % +Ps, +Index, +Name, -Reply
    get_settings/5,         % +Ps, +Index, +Name, +Params, -Reply
    put_settings/4,         % +Ps, +Index, +Body, -Reply
    put_settings/5,         % +Ps, +Index, +Params, +Body, -Reply
    put_warmer/6,           % +Ps, +Index, +DocType, +Name, +Body, -Reply
    put_warmer/7,           % +Ps, +Index, +DocType, +Name, +Params, +Body, -Reply
    get_warmer/5,           % +Ps, +Index, +DocType, +Name, -Reply
    get_warmer/6,           % +Ps, +Index, +DocType, +Name, +Params, -Reply
    delete_warmer/4,        % +Ps, +Index, +Name, -Reply
    delete_warmer/5,        % +Ps, +Index, +Name, +Params, -Reply
    status/3,               % +Ps, +Index, -Reply
    status/4,               % +Ps, +Index, +Params, -Reply
    stats/4,                % +Ps, +Index, +Metric, -Reply
    stats/5,                % +Ps, +Index, +Metric, +Params, -Reply
    segments/3,             % +Ps, +Index, -Reply
    segments/4,             % +Ps, +Index, +Params, -Reply
    optimize/3,             % +Ps, +Index, -Reply
    optimize/4,             % +Ps, +Index, +Params, -Reply
    validate_query/5,       % +Ps, +Index, +DocType, +Body, -Reply
    validate_query/6,       % +Ps, +Index, +DocType, +Params, +Body, -Reply
    clear_cache/3,          % +Ps, +Index, -Reply
    clear_cache/4,          % +Ps, +Index, +Params, -Reply
    recovery/3,             % +Ps, +Index, -Reply
    recovery/4,             % +Ps, +Index, +Params, -Reply
    snapshot_index/3,       % +Ps, +Index, -Reply
    snapshot_index/4,       % +Ps, +Index, +Params, -Reply
    upgrade/3,              % +Ps, +Index, -Reply
    upgrade/4,              % +Ps, +Index, +Params, -Reply
    get_upgrade/3,          % +Ps, +Index, -Reply
    get_upgrade/4           % +Ps, +Index, +Params, -Reply
]).

/** <module> Indices APIs
The indices APIs are used to manage individual indices,
index settings, aliases, mappings, index templates and warmers.

@author Hongxin Liang
@license Apache License Version 2.0
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
    perform_request(Ps, get, Context, Params, Body, _, Reply).

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

%% get_field_mapping(+Ps, +Index, +DocType, +Field, -Reply) is semidet.
%% get_mapping(+Ps, +Index, +DocType, +Field, +Params, -Reply) is semidet.
%
% Retrieve mapping definition of a specific field.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-get-field-mapping.html).

get_field_mapping(Ps, Index, DocType, Field, Reply) :-
    get_field_mapping(Ps, Index, DocType, Field, _{}, Reply).

get_field_mapping(Ps, Index, DocType, Field, Params, Reply) :-
    non_empty(Field, field),
    make_context([Index, '_mapping', DocType, field, Field], Context),
    perform_request(Ps, get, Context, Params,  _, Reply).

%% delete_mapping(+Ps, +Index, +DocType, -Reply) is semidet.
%% delete_mapping(+Ps, +Index, +DocType, +Params, -Reply) is semidet.
%
% Delete a mapping (type) along with its data.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-delete-mapping.html).

delete_mapping(Ps, Index, DocType, Reply) :-
    delete_mapping(Ps, Index, DocType, _{}, Reply).

delete_mapping(Ps, Index, DocType, Params, Reply) :-
    forall(member(Value-Name, [Index-index, DocType-doc_type]), non_empty(Value, Name)),
    make_context([Index, '_mapping', DocType], Context),
    perform_request(Ps, delete, Context, Params,  _, Reply).

%% put_alias(+Ps, +Index, +Alias, +Body, -Reply) is semidet.
%% get_mapping(+Ps, +Index, +Alias, +Params, +Body, -Reply) is semidet.
%
% Create an alias for a specific index/indices.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-aliases.html).

put_alias(Ps, Index, Alias, Body, Reply) :-
    put_alias(Ps, Index, Alias, _{}, Body, Reply).

put_alias(Ps, Index, Alias, Params, Body, Reply) :-
    forall(member(Value-Name, [Index-index, Alias-alias]), non_empty(Value, Name)),
    make_context([Index, '_alias', Alias], Context),
    perform_request(Ps, put, Context, Params, Body, _, Reply).

%% exists_alias(+Ps, +Index, +Alias) is semidet.
%% exists_alias(+Ps, +Index, +Alias, +Params) is semidet.
%
% Return a boolean indicating whether given alias exists.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-aliases.html).

exists_alias(Ps, Index, Alias) :-
    exists_alias(Ps, Index, Alias, _{}).

exists_alias(Ps, Index, Alias, Params) :-
    make_context([Index, '_alias', Alias], Context),
    (   catch(perform_request(Ps, head, Context, Params, _, _), E, true)
    ->  (   var(E)
        ->  true
        ;   E = plasticsearch_exception(404, _)
        )
    ).

%% get_alias(+Ps, +Index, +Alias, -Reply) is semidet.
%% get_alias(+Ps, +Index, +Alias, +Params, -Reply) is semidet.
%
% Retrieve a specified alias.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-aliases.html).

get_alias(Ps, Index, Alias, Reply) :-
    get_alias(Ps, Index, Alias, _{}, Reply).

get_alias(Ps, Index, Alias, Params, Reply) :-
    make_context([Index, '_alias', Alias], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% get_aliases(+Ps, +Index, +Alias, -Reply) is semidet.
%% get_aliases(+Ps, +Index, +Alias, +Params, -Reply) is semidet.
%
% Retrieve a specified aliases.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-aliases.html).

get_aliases(Ps, Index, Aliases, Reply) :-
    get_aliases(Ps, Index, Aliases, _{}, Reply).

get_aliases(Ps, Index, Aliases, Params, Reply) :-
    make_context([Index, '_aliases', Aliases], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% update_aliases(+Ps, +Body, -Reply) is semidet.
%% update_aliases(+Ps, +Params, +Body, -Reply) is semidet.
%
% Update specified aliases.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-aliases.html).

update_aliases(Ps, Body, Reply) :-
    update_aliases(Ps, _{}, Body, Reply).

update_aliases(Ps, Params, Body, Reply) :-
    non_empty(Body, body),
    perform_request(Ps, post, '/_aliases', Params, Body, _, Reply).

%% delete_alias(+Ps, +Index, +Alias, -Reply) is semidet.
%% delete_alias(+Ps, +Index, +Alias, +Params, -Reply) is semidet.
%
% Delete specific alias.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-aliases.html).

delete_alias(Ps, Index, Alias, Reply) :-
    delete_alias(Ps, Index, Alias, _{}, Reply).

delete_alias(Ps, Index, Alias, Params, Reply) :-
    forall(member(Value-Name, [Index-index, Alias-alias]), non_empty(Value, Name)),
    make_context([Index, '_alias', Alias], Context),
    perform_request(Ps, delete, Context, Params,  _, Reply).

%% put_template(+Ps, +Name, +Body, -Reply) is semidet.
%% put_template(+Ps, +Name, +Params, +Body, -Reply) is semidet.
%
% Create an index template that will automatically be applied to new
% indices created.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-templates.html).

put_template(Ps, Name, Body, Reply) :-
    put_template(Ps, Name, _{}, Body, Reply).

put_template(Ps, Name, Params, Body, Reply) :-
    forall(member(Value-Name, [Name-name, Body-body]), non_empty(Value, Name)),
    make_context(['_template', Name], Context),
    perform_request(Ps, put, Context, Params, Body, _, Reply).

%% exists_template(+Ps, +Name) is semidet.
%% exists_template(+Ps, +Name, +Params) is semidet.
%
% Return a boolean indicating whether given template exists.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-templates.html).

exists_template(Ps, Name) :-
    exists_template(Ps, Name, _{}).

exists_template(Ps, Name, Params) :-
    non_empty(Name, name),
    make_context(['_template', Name], Context),
    (   catch(perform_request(Ps, head, Context, Params, _, _), E, true)
    ->  (   var(E)
        ->  true
        ;   E = plasticsearch_exception(404, _)
        )
    ).

%% get_template(+Ps, +Name, -Reply) is semidet.
%% get_template(+Ps, +Name, +Params, -Reply) is semidet.
%
% Retrieve an index template by its name.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-templates.html).

get_template(Ps, Name, Reply) :-
    get_template(Ps, Name, _{}, Reply).

get_template(Ps, Name, Params, Reply) :-
    make_context(['_template', Name], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% delete_template(+Ps, +Name, -Reply) is semidet.
%% delete_template(+Ps, +Name, +Params, -Reply) is semidet.
%
% Delete an index template by its name.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-templates.html).

delete_template(Ps, Name, Reply) :-
    delete_template(Ps, Name, _{}, Reply).

delete_template(Ps, Name, Params, Reply) :-
    non_empty(Name, name),
    make_context(['_template', Name], Context),
    perform_request(Ps, delete, Context, Params, _, Reply).

%% get_settings(+Ps, +Index, +Name, -Reply) is semidet.
%% get_settings(+Ps, +Index, +Name, +Params, -Reply) is semidet.
%
% Retrieve settings for one or more (or all) indices.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-get-settings.html).

get_settings(Ps, Index, Name, Reply) :-
    get_settings(Ps, Index, Name, _{}, Reply).

get_settings(Ps, Index, Name, Params, Reply) :-
    make_context([Index, '_settings', Name], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% put_settings(+Ps, +Index, +Body, -Reply) is semidet.
%% put_settings(+Ps, +Index, +Params, +Body, -Reply) is semidet.
%
% Change specific index level settings in real time.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-update-settings.html).

put_settings(Ps, Index, Body, Reply) :-
    put_settings(Ps, Index, _{}, Body, Reply).

put_settings(Ps, Index, Params, Body, Reply) :-
    non_empty(Body, body),
    make_context([Index, '_settings'], Context),
    perform_request(Ps, put, Context, Params, Body, _, Reply).

%% put_warmer(+Ps, +Index, +DocType, +Name, +Body, -Reply) is semidet.
%% put_warmer(+Ps, +Index, +DocType, +Name, +Params, +Body, -Reply) is semidet.
%
% Create an index warmer to run registered search requests to warm up the
% index before it is available for search.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-warmers.html).

put_warmer(Ps, Index, DocType, Name, Body, Reply) :-
    put_warmer(Ps, Index, DocType, Name, _{}, Body, Reply).

put_warmer(Ps, Index, DocType, Name, Params, Body, Reply) :-
    forall(member(Value-Name, [Name-name, Body-body]), non_empty(Value, Name)),
    (   Index = '', DocType \= ''
    ->  Index1 = '_all'
    ;   Index1 = Index
    ),
    make_context([Index1, DocType, '_warmer', Name], Context),
    perform_request(Ps, put, Context, Params, Body, _, Reply).

%% get_warmer(+Ps, +Index, +DocType, +Name, -Reply) is semidet.
%% get_warmer(+Ps, +Index, +DocType, +Name, +Params, -Reply) is semidet.
%
% Retrieve an index warmer.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-warmers.html).

get_warmer(Ps, Index, DocType, Name, Reply) :-
    get_warmer(Ps, Index, DocType, Name, _{}, Reply).

get_warmer(Ps, Index, DocType, Name, Params, Reply) :-
    make_context([Index, DocType, '_warmer', Name], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% delete_warmer(+Ps, +Index, +Name, -Reply) is semidet.
%% delete_warmer(+Ps, +Index, +Name, +Params, -Reply) is semidet.
%
% Delete an index warmer.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-warmers.html).

delete_warmer(Ps, Index, Name, Reply) :-
    delete_warmer(Ps, Index, Name, _{}, Reply).

delete_warmer(Ps, Index, Name, Params, Reply) :-
    forall(member(Value-Name, [Index-index, Name-name]), non_empty(Value, Name)),
    make_context([Index, '_warmer', Name], Context),
    perform_request(Ps, delete, Context, Params, _, Reply).

%% status(+Ps, +Index, -Reply) is semidet.
%% status(+Ps, +Index, +Params, -Reply) is semidet.
%
% Get a comprehensive status information of one or more indices.
% See [here](http://elastic.co/guide/reference/api/admin-indices-_/).

status(Ps, Index, Reply) :-
    status(Ps, Index, _{}, Reply).

status(Ps, Index, Params, Reply) :-
    make_context([Index, '_status'], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% stats(+Ps, +Index, +Metric, -Reply) is semidet.
%% stats(+Ps, +Index, +Metric, +Params, -Reply) is semidet.
%
% Retrieve statistics on different operations happening on an index.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-stats.html).

stats(Ps, Index, Metric, Reply) :-
    stats(Ps, Index, Metric, _{}, Reply).

stats(Ps, Index, Metric, Params, Reply) :-
    make_context([Index, '_stats', Metric], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% segments(+Ps, +Index, -Reply) is semidet.
%% segments(+Ps, +Index, +Params, -Reply) is semidet.
%
% Provide low level segments information that a Lucene index (shard level) is built with.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-segments.html).

segments(Ps, Index, Reply) :-
    segments(Ps, Index, _{}, Reply).

segments(Ps, Index, Params, Reply) :-
    make_context([Index, '_segments'], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% optimize(+Ps, +Index, -Reply) is semidet.
%% optimize(+Ps, +Index, +Params, -Reply) is semidet.
%
% Explicitly optimize one or more indices through an API.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-optimize.html).

optimize(Ps, Index, Reply) :-
    optimize(Ps, Index, _{}, Reply).

optimize(Ps, Index, Params, Reply) :-
    make_context([Index, '_optimize'], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% validate_query(+Ps, +Index, +DocType, +Body, -Reply) is semidet.
%% validate_query(+Ps, +Index, +DocType, +Params, +Body, -Reply) is semidet.
%
% Validate a potentially expensive query without executing it.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/search-validate.html).

validate_query(Ps, Index, DocType, Body, Reply) :-
    validate_query(Ps, Index, DocType, _{}, Body, Reply).

validate_query(Ps, Index, DocType, Params, Body, Reply) :-
    make_context([Index, DocType, '_validate', query], Context),
    perform_request(Ps, get, Context, Params, Body, _, Reply).

%% clear_cache(+Ps, +Index, -Reply) is semidet.
%% clear_cache(+Ps, +Index, +Params, -Reply) is semidet.
%
% Clear either all caches or specific cached associated with one ore more indices.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-clearcache.html).

clear_cache(Ps, Index, Reply) :-
    clear_cache(Ps, Index, _{}, Reply).

clear_cache(Ps, Index, Params, Reply) :-
    make_context([Index, '_cache', clear], Context),
    perform_request(Ps, post, Context, Params, '', _, Reply).

%% recovery(+Ps, +Index, -Reply) is semidet.
%% recovery(+Ps, +Index, +Params, -Reply) is semidet.
%
% The indices recovery API provides insight into on-going shard
% recoveries. Recovery status may be reported for specific indices, or
% cluster-wide.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/master/indices-recovery.html).

recovery(Ps, Index, Reply) :-
    recovery(Ps, Index, _{}, Reply).

recovery(Ps, Index, Params, Reply) :-
    make_context([Index, '_recovery'], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% snapshot_index(+Ps, +Index, -Reply) is semidet.
%% snapshot_index(+Ps, +Index, +Params, -Reply) is semidet.
%
% Explicitly perform a snapshot through the gateway of one or more indices (backup them).
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-gateway-snapshot.html).

snapshot_index(Ps, Index, Reply) :-
    snapshot_index(Ps, Index, _{}, Reply).

snapshot_index(Ps, Index, Params, Reply) :-
    make_context([Index, '_gateway', 'snapshot'], Context),
    perform_request(Ps, post, Context, Params, '', _, Reply).

%% upgrade(+Ps, +Index, -Reply) is semidet.
%% upgrade(+Ps, +Index, +Params, -Reply) is semidet.
%
% Upgrade one or more indices to the latest format through an API.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-upgrade.html).

upgrade(Ps, Index, Reply) :-
    upgrade(Ps, Index, _{}, Reply).

upgrade(Ps, Index, Params, Reply) :-
    make_context([Index, '_upgrade'], Context),
    perform_request(Ps, post, Context, Params, '', _, Reply).

%% get_upgrade(+Ps, +Index, -Reply) is semidet.
%% get_upgrade(+Ps, +Index, +Params, -Reply) is semidet.
%
% Monitor how much of one or more index is upgraded.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-upgrade.html).

get_upgrade(Ps, Index, Reply) :-
    get_upgrade(Ps, Index, _{}, Reply).

get_upgrade(Ps, Index, Params, Reply) :-
    make_context([Index, '_upgrade'], Context),
    perform_request(Ps, get, Context, Params, _, Reply).
