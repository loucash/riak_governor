-module(riak_governor).

-export([start/0, stop/0]).
-export([get_env/1, get_env/2]).
-export([get_cluster_leader/0, get_leader/1, is_leader/1, is_leader/2]).

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

get_cluster_leader() ->
    Nodes = riak_governor_util:get_cluster_nodes(),
    Name  = riak_governor_util:ensemble_name(Nodes),
    find_leader(Name, Nodes).

is_leader(Key) ->
    is_leader(node(), Key).

is_leader(Node, Key) ->
    case get_leader(Key) of
        {ok, Node} -> true;
        _ -> false
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
find_leader(_Name, [Node]) ->
    {ok, Node};
find_leader(Name, Nodes) ->
    do_find_leader(ordsets:is_element(node(), Nodes), Name, Nodes).

do_find_leader(_IsLocal, _Name, []) ->
    {error, noproc};
do_find_leader(true, Name, Nodes) ->
    case catch rafter:get_leader(Name) of
        {'EXIT', {noproc, _}} ->
            do_find_leader(false, Name, Nodes -- [node()]);
        {_, Result} ->
            {ok, Result};
        undefined ->
            {error, no_leader}
    end;
do_find_leader(false, Name, [Node|Rest]) ->
    case catch rafter:get_leader({Name, Node}) of
        {'EXIT', {noproc, _}} ->
            do_find_leader(false, Name, Rest);
        {_, Result} ->
            {ok, Result};
        undefined ->
            {error, no_leader}
    end.
