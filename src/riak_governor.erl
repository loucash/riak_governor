-module(riak_governor).

-export([start/0, stop/0]).
-export([get_env/1, get_env/2]).
-export([is_leader/1, is_leader/2, get_leader/1, get_leader/2]).

-define(APP, ?MODULE).

start() ->
    reltool_util:application_start(?APP).

stop() ->
    reltool_util:application_stop(?APP).

is_leader(Key) ->
    is_leader(node(), Key).

is_leader(Node, Key) ->
    MaybeLeader = get_leader(Node, Key),
    MaybeLeader =/= no_leader andalso MaybeLeader =:= node().

get_leader(Key) ->
    get_leader(node(), Key).

get_leader(Node, Key) ->
    DocIdx   = riak_core_util:chash_key(Key),
    Preflist = riak_governor_util:get_primary_apl(DocIdx),
    Nodes    = lists:usort([PrefNode || {{_Index, PrefNode}, _Type} <- Preflist]),
    case Nodes of
        [Node] -> Node;
        [_] -> no_leader;
        [_|_] ->
            Name = riak_governor_util:ensemble_name(Nodes),
            ProcId = case ordsets:is_element(node(), Nodes) of
                         true -> Name;
                         false ->
                             [First|_] = Nodes,
                             {Name, First}
                     end,
            case catch rafter:get_leader(ProcId) of
                {_, Node} -> Node;
                {'EXIT', {noproc, _}} -> no_leader;
                _ -> no_leader
            end
    end.

get_env(Name) ->
    application:get_env(?APP, Name).

get_env(Name, Default) ->
    application:get_env(?APP, Name, Default).
