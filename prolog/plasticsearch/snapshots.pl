:- module(snapshots, [
    create/5,               % +Ps, +Repository, +Snapshots, +Body, -Reply
    create/6,               % +Ps, +Repository, +Snapshots, +Params, +Body, -Reply
    delete/4,               % +Ps, +Repository, +Snapshots, -Reply
    delete/5,               % +Ps, +Repository, +Snapshots, +Params, -Reply
    get/4,                  % +Ps, +Repository, +Snapshots, -Reply
    get/5,                  % +Ps, +Repository, +Snapshots, +Params, -Reply
    delete_repository/3,    % +Ps, +Repository, -Reply
    delete_repository/4,    % +Ps, +Repository, +Params, -Reply
    get_repository/3,       % +Ps, +Repository, -Reply
    get_repository/4,       % +Ps, +Repository, +Params, -Reply
    create_repository/4,    % +Ps, +Repository, +Body, -Reply
    create_repository/5,    % +Ps, +Repository, +Params, +Body, -Reply
    restore/5,              % +Ps, +Repository, +Snapshots, +Body, -Reply
    restore/6,              % +Ps, +Repository, +Snapshots, +Params, +Body, -Reply
    status/4,               % +Ps, +Repository, +Snapshots, -Reply
    status/5,               % +Ps, +Repository, +Snapshots, +Params, -Reply
    verify_repository/3,    % +Ps, +Repository, -Reply
    verify_repository/4     % +Ps, +Repository, +Params, -Reply
]).

/** <module> Snapshots APIs
The snapshot and restore module allows to create snapshots of individual indices or an entire cluster into a remote repository.

@auther Hongxin Liang
@license Apache License Version 2.0
@see http://www.elastic.co/guide/en/elasticsearch/reference/master/modules-snapshots.html
*/

%% create(+Repository, +Snapshot, +Body, -Reply) is semidet.
%% create(+Repository, +Snapshot, +Params, +Body, -Reply) is semidet.
%
% Create a snapshot in repository.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/master/modules-snapshots.html).

create(Ps, Repository, Snapshot, Body, Reply) :-
    create(Ps, Repository, Snapshot, _{}, Body, Reply).

create(Ps, Repository, Snapshot, Params, Body, Reply) :-
    forall(member(Value-Name, [Repository-repository, Snapshot-snapshot]), non_empty(Value, Name)),
    make_context(['_snapshot', Repository, Snapshot], Context),
    perform_request(Ps, put, Context, Params, Body, _, Reply).

%% delete(+Repository, +Snapshot, -Reply) is semidet.
%% delete(+Repository, +Snapshot, +Params, -Reply) is semidet.
%
% Delete a snapshot from repository.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/master/modules-snapshots.html).

delete(Ps, Repository, Snapshot, Reply) :-
    delete(Ps, Repository, Snapshot, _{}, Reply).

delete(Ps, Repository, Snapshot, Params, Reply) :-
    forall(member(Value-Name, [Repository-repository, Snapshot-snapshot]), non_empty(Value, Name)),
    make_context(['_snapshot', Repository, Snapshot], Context),
    perform_request(Ps, delete, Context, Params, _, Reply).

%% get(+Repository, +Snapshot, -Reply) is semidet.
%% get(+Repository, +Snapshot, +Params, -Reply) is semidet.
%
% Retrieve information about a snapshot.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/master/modules-snapshots.html).

get(Ps, Repository, Snapshot, Reply) :-
    get(Ps, Repository, Snapshot, _{}, Reply).

get(Ps, Repository, Snapshot, Params, Reply) :-
    forall(member(Value-Name, [Repository-repository, Snapshot-snapshot]), non_empty(Value, Name)),
    make_context(['_snapshot', Repository, Snapshot], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% delete_repository(+Repository, -Reply) is semidet.
%% delete_repository(+Repository, +Params, -Reply) is semidet.
%
% Removes a shared file system repository.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/master/modules-snapshots.html).

delete_repository(Ps, Repository, Reply) :-
    delete_repository(Ps, Repository, _{}, Reply).

delete_repository(Ps, Repository, Params, Reply) :-
    non_empty(Repository, repository),
    make_context(['_snapshot', Repository], Context),
    perform_request(Ps, delete, Context, Params, _, Reply).

%% get_repository(+Repository, -Reply) is semidet.
%% get_repository(+Repository, +Params, -Reply) is semidet.
%
% Return information about registered repositories.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/master/modules-snapshots.html).

get_repository(Ps, Repository, Reply) :-
    get_repository(Ps, Repository, _{}, Reply).

get_repository(Ps, Repository, Params, Reply) :-
    make_context(['_snapshot', Repository], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% create_repository(+Repository, +Body, -Reply) is semidet.
%% create_repository(+Repository, +Params, +Body, -Reply) is semidet.
%
% Registers a shared file system repository.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/master/modules-snapshots.html).

create_repository(Ps, Repository, Body, Reply) :-
    create_repository(Ps, Repository, _{}, Body, Reply).

create_repository(Ps, Repository, Params, Body, Reply) :-
    forall(member(Value-Name, [Repository-repository, Body-body]), non_empty(Value, Name)),
    make_context(['_snapshot', Repository], Context),
    perform_request(Ps, put, Context, Params, Body, _, Reply).

%% restore(+Repository, +Snapshot, +Body, -Reply) is semidet.
%% restore(+Repository, +Snapshot, +Params, +Body, -Reply) is semidet.
%
% Restore a snapshot in repository.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/master/modules-snapshots.html).

restore(Ps, Repository, Snapshot, Body, Reply) :-
    restore(Ps, Repository, Snapshot, _{}, Body, Reply).

restore(Ps, Repository, Snapshot, Params, Body, Reply) :-
    forall(member(Value-Name, [Repository-repository, Snapshot-snapshot]), non_empty(Value, Name)),
    make_context(['_snapshot', Repository, Snapshot, '_restore'], Context),
    perform_request(Ps, post, Context, Params, Body, _, Reply).

%% status(+Repository, +Snapshot, -Reply) is semidet.
%% status(+Repository, +Snapshot, +Params, -Reply) is semidet.
%
% Return information about all currently running snapshots. By specifying
% a repository name, it's possible to limit the results to a particular
% repository.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/master/modules-snapshots.html).

status(Ps, Repository, Snapshot, Reply) :-
    status(Ps, Repository, Snapshot, _{}, Reply).

status(Ps, Repository, Snapshot, Params, Reply) :-
    make_context(['_snapshot', Repository, Snapshot, '_status'], Context),
    perform_request(Ps, get, Context, Params, _, Reply).

%% verify_repository(+Repository, -Reply) is semidet.
%% verify_repository(+Repository, +Params, -Reply) is semidet.
%
% Returns a list of nodes where repository was successfully verified or
% an error message if verification process failed.
% See [here](http://www.elastic.co/guide/en/elasticsearch/reference/master/modules-snapshots.html).

verify_repository(Ps, Repository, Reply) :-
    verify_repository(Ps, Repository, _{}, Reply).

verify_repository(Ps, Repository, Params, Reply) :-
    non_empty(Repository, repository),
    make_context(['_snapshot', Repository, '_verify'], Context),
    perform_request(Ps, post, Context, Params, '', _, Reply).
