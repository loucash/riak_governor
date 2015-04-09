-module(rt_governor).

-export([
         get_leader/2,
         get_cluster_leader/1,
         is_leader/2,
         is_leader/3,
         wait_for_leader/1,
         wait_for_no_leader/1,
         stop/1
         ]).


get_leader(Node, Key) ->
    rpc:call(Node, riak_governor, get_leader, [Key]).

get_cluster_leader(Node) ->
    rpc:call(Node, riak_governor, get_cluster_leader, []).

is_leader(Node, Key) ->
    rpc:call(Node, riak_governor, is_leader, [Key]).

is_leader(Node, ANode, Key) ->
    rpc:call(Node, riak_governor, is_leader, [ANode, Key]).

wait_for_leader(Node) ->
    lager:info("Wait until cluster leader using ~p", [Node]),
    ok = rt:wait_until(
           fun() ->
                   case get_cluster_leader(Node) of
                       {ok, _} -> true;
                       _ -> false
                   end
           end, 30, 200).

wait_for_no_leader(Node) ->
    lager:info("Wait until no cluster leader using ~p", [Node]),
    ok = rt:wait_until(
           fun() ->
                   case get_cluster_leader(Node) of
                       {error, no_leader} -> true;
                       _ -> false
                   end
           end).

stop(Node) ->
    lager:info("Stopping riak_governor on ~p", [Node]),
    riak_governor_harness:stop(Node).
