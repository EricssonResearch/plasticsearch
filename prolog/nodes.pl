:- module(nodes, [
    info/4,         % +Ps, +NodeID, +Metric, -Reply
    info/5,         % +Ps, +NodeID, +Metric, +Params, -Reply
    shutdown/3,     % +Ps, +NodeID, -Reply
    shutdown/4,     % +Ps, +NodeID, +Params, -Reply
    stats/5,        % +Ps, +NodeID, +Metric, +IndexMetric, -Reply
    stats/6,        % +Ps, +NodeID, +Metric, +IndexMetric, +Params, -Reply
    hot_threads/3,  % +Ps, +NodeID, -Reply
    hot_threads/4   % +Ps, +NodeID, +Params, -Reply
]).

/** <module> Nodes APIs
Manage nodes.

@auther Hongxin Liang
@license TBD
@see http://www.elastic.co/guide/en/elasticsearch/reference/current/cluster.html
*/

:- use_module(transport).
:- use_module(util).

%% info(+Ps, +NodeID, +Metric, -Reply) is semidet.
%% info(+Ps, +NodeID, +Metric, +Params, -Reply) is semidet.
%
% The cluster nodes info API allows to retrieve one or more (or all) of
% the cluster nodes information.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-nodes-info.html).

info(Ps, NodeID, Metric, Reply) :-
    info(Ps, NodeID, Metric, _{}, Reply).

info(Ps, NodeID, Metric, Params, Reply) :-
    make_context(['_nodes', NodeID, Metric], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% shutdown(+Ps, +NodeID, -Reply) is semidet.
%% shutdown(+Ps, +NodeID, +Params, -Reply) is semidet.
%
% The nodes shutdown API allows to shutdown one or more (or all) nodes in
% the cluster.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-nodes-shutdown.html).

shutdown(Ps, NodeID, Reply) :-
    shutdown(Ps, NodeID, _{}, Reply).

shutdown(Ps, NodeID, Params, Reply) :-
    make_context(['_cluster', 'nodes', NodeID, '_shutdown'], Context),
    perform_request(Ps, post, Context, Params, '', _, Reply).

%% stats(+Ps, +NodeID, +Metric, +IndexMetric, -Reply) is semidet.
%% stats(+Ps, +NodeID, +Metric, +IndexMetric, Params, -Reply) is semidet.
%
% The cluster nodes stats API allows to retrieve one or more (or all) of
% the cluster nodes statistics.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-nodes-stats.html).

stats(Ps, NodeID, Metric, IndexMetric, Reply) :-
    stats(Ps, NodeID, Metric, IndexMetric, _{}, Reply).

stats(Ps, NodeID, Metric, IndexMetric, Params, Reply) :-
    make_context(['_nodes', NodeID, 'stats', Metric, IndexMetric], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% hot_threads(+Ps, +NodeID, -Reply) is semidet.
%% hot_threads(+Ps, +NodeID, +Params, -Reply) is semidet.
%
% An API allowing to get the current hot threads on each node in the cluster.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/current/cluster-nodes-hot-threads.html).

hot_threads(Ps, NodeID, Reply) :-
    hot_threads(Ps, NodeID, _{}, Reply).

hot_threads(Ps, NodeID, Params, Reply) :-
    make_context(['_nodes', NodeID, 'hot_threads'], Context),
    perform_request(Ps, get, Context, Params, _, Reply).
