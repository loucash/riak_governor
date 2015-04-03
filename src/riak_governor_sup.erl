-module(riak_governor_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Type), {Id, {Id, start_link, []},
                          permanent, 5000, Type, [Id]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    VNode = {riak_governor_vnode_master,
             {riak_core_vnode_master, start_link, [riak_governor_vnode]},
             permanent, 5000, worker, [riak_core_vnode_master]},
    {ok, {{one_for_one, 5, 10},
          [VNode,
           ?CHILD(riak_governor_ensemble_master, worker)]}}.
