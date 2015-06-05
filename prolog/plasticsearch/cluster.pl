:- module(cluster, [
    health/3,          % +Ps, +Index, -Reply
    health/4,          % +Ps, +Index, +Params, -Reply
    pending_tasks/2,   % +Ps, +Index, -Reply
    pending_tasks/3,   % +Ps, +Index, +Params, -Reply
    state/4,           % +Ps, +Index, +Metric, -Reply
    state/5,           % +Ps, +Index, +Metric, +Params, -Reply
    stats/3,           % +Ps, +NodeID, -Reply
    stats/4,           % +Ps, +NodeID, +Params, -Reply
    reroute/3,         % +Ps, +Body, -Reply
    reroute/4,         % +Ps, +Params, +Body, -Reply
    get_settings/2,    % +Ps, -Reply
    get_settings/3,    % +Ps, +Params, -Reply
    put_settings/3,    % +Ps, +Body, -Reply
    put_settings/4     % +Ps, +Params, +Body, -Reply
]).

/** <module> Cluster APIs
Manage cluster.

@auther Hongxin Liang
@license TBD
@see http://www.elastic.co/guide/en/elasticsearch/reference/current/cluster.html
*/

:- use_module(transport).
:- use_module(util).

%% health(+Ps, +Index, -Reply) is semidet.
%% health(+Ps, +Index, +Params, -Reply) is semidet.
%
% Get a very simple status on the health of the cluster.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-health.html).

health(Ps, Index, Reply) :-
    health(Ps, Index, _{}, Reply).

health(Ps, Index, Params, Reply) :-
    make_context(['_cluster', health, Index], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% pending_tasks(+Ps, -Reply) is semidet.
%% pending_tasks(+Ps, +Params, -Reply) is semidet.
%
% The pending cluster tasks API returns a list of any cluster-level
% changes (e.g. create index, update mapping, allocate or fail shard)
% which have not yet been executed.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-pending.html).

pending_tasks(Ps, Reply) :-
    pending_tasks(Ps, _{}, Reply).

pending_tasks(Ps, Params, Reply) :-
    perform_request(Ps, get, '/_cluster/pending_tasks', Params, _, Reply).

%% state(+Ps, +Index, +Metric, -Reply) is semidet.
%% state(+Ps, +Index, +Metric, +Params, -Reply) is semidet.
%
% Get a comprehensive state information of the whole cluster.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-state.html).

state(Ps, Index, Metric, Reply) :-
    state(Ps, Index, Metric, _{}, Reply).

state(Ps, Index, Metric, Params, Reply) :-
    (   Metric = '', Index \= ''
    ->  Metric1 = '_all'
    ;   Metric1 = Metric
    ),
    make_context(['_cluster', 'state', Metric1, Index], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% stats(+Ps, +NodeID, -Reply) is semidet.
%% stats(+Ps, +NodeID, +Params, -Reply) is semidet.
%
% The Cluster Stats API allows to retrieve statistics from a cluster wide
% perspective. The API returns basic index metrics and information about
% the current nodes that form the cluster.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-stats.html).

stats(Ps, NodeID, Reply) :-
    stats(Ps, NodeID, _{}, Reply).

stats(Ps, NodeID, Params, Reply) :-
    (   NodeID = ''
    ->  Context = '/_cluster/stats'
    ;   make_context(['_cluster/stats/nodes', NodeID], Context)
    ),
    perform_request(Ps, get, Context, Params, _, Reply).

%% reroute(+Ps, +Body, -Reply) is semidet.
%% reroute(+Ps, +Params, +Body, -Reply) is semidet.
%
% Explicitly execute a cluster reroute allocation command including specific commands.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-reroute.html).

reroute(Ps, Body, Reply) :-
    reroute(Ps, _{}, Body, Reply).

reroute(Ps, Params, Body, Reply) :-
    perform_request(Ps, post, '/_cluster/reroute', Params, Body, _, Reply).

%% get_settings(+Ps, -Reply) is semidet.
%% get_settings(+Ps, +Params, -Reply) is semidet.
%
% Get cluster settings.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-update-settings.html).

get_settings(Ps, Reply) :-
    get_settings(Ps, _{}, Reply).

get_settings(Ps, Params, Reply) :-
    perform_request(Ps, get, '/_cluster/settings', Params, _, Reply).

%% put_settings(+Ps, +Body, -Reply) is semidet.
%% put_settings(+Ps, +Params, +Body, -Reply) is semidet.
%
% Update cluster wide specific settings.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-update-settings.html).

put_settings(Ps, Body, Reply) :-
    put_settings(Ps, _{}, Body, Reply).

put_settings(Ps, Params, Body, Reply) :-
    perform_request(Ps, put, '/_cluster/settings', Params, Body, _, Reply).
