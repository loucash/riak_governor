-module(riak_governor).

-export([start/0, stop/0]).
-export([get_env/1, get_env/2]).
-export([get_leader/1, is_leader/1, is_leader/2]).
-export([get_cluster_leader/0, is_global_leader/0, is_global_leader/1]).

-define(APP, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    reltool_util:application_start(?APP).

stop() ->
    reltool_util:application_stop(?APP).

get_env(Name) ->
    application:get_env(?APP, Name).

get_env(Name, Default) ->
    application:get_env(?APP, Name, Default).

get_leader(Key) ->
    DocIdx   = riak_core_util:chash_key(Key),
    Preflist = riak_governor_util:get_primary_apl(DocIdx),
    Nodes    = lists:usort([PrefNode || {{_Index, PrefNode}, _Type} <- Preflist]),
    Name     = riak_governor_util:ensemble_name(Nodes),
    find_leader(Name, Nodes).

is_leader(Key) ->
    is_leader(node(), Key).

is_leader(Node, Key) ->
    case get_leader(Key) of
        {ok, Node} -> true;
        _ -> false
    end.

get_cluster_leader() ->
    Nodes = riak_governor_util:get_cluster_nodes(),
    Name  = riak_governor_util:ensemble_name(Nodes),
    find_leader(Name, Nodes).

is_global_leader() ->
    is_global_leader(node()).

is_global_leader(Node) ->
    case get_cluster_leader() of
        {ok, Node} -> true;
        _ -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
find_leader(_Name, [Node]) ->
    {ok, Node};
find_leader(Name, Nodes) ->
    case ordsets:is_element(node(), Nodes) of
        true ->
            do_find_leader(Name, local_node_first(Nodes));
        false ->
            do_find_leader(Name, Nodes)
    end.

local_node_first(Nodes) ->
    LocalNode = node(),
    [LocalNode | Nodes -- [LocalNode]].

do_find_leader(_Name, []) ->
    {error, noproc};
do_find_leader(Name, [Node|Rest]) ->
    case catch riak_governor_spi:get_leader({Name, Node}) of
        {'EXIT', {noproc, _}} ->
            do_find_leader(Name, Rest);
        {'EXIT', {{nodedown,_}, _}} ->
            do_find_leader(Name, Rest);
        {_, Result} ->
            {ok, Result};
        undefined ->
            {error, no_leader}
    end.
