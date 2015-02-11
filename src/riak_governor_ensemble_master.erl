-module(riak_governor_ensemble_master).
-behaviour(gen_server).

%% API
-export([ring_changed/0]).
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {
          ensemble_size :: pos_integer(),
          ringhash      :: binary()
         }).

%%%===================================================================
%%% API
%%%===================================================================
ring_changed() ->
    gen_server:cast(?MODULE, ring_changed).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    EnsembleSize = riak_governor_util:get_ensemble_size(),
    RingHash = get_ring_hash(),
    _ = ets:new(?MODULE, [named_table, public]),
    timer:apply_after(500, gen_server, cast, [?MODULE, init_ensembles]),
    {ok, #state{ensemble_size=EnsembleSize,
                ringhash=RingHash}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(ring_changed, #state{ringhash=RingHash}=State) ->
    case get_ring_hash() of
        RingHash ->
            {noreply, State};
        NewRingHash ->
            ensure_ensembles_started(State),
            {noreply, State#state{ringhash=NewRingHash}}
    end;
handle_cast(init_ensembles, State) ->
    ensure_ensembles_started(State),
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
get_ring_hash() ->
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    crypto:hash(md5, term_to_binary(Ring)).

ensure_ensembles_started(#state{ensemble_size=EnsembleSize}) ->
    Ensembles0 = all_ensembles(EnsembleSize),
    ClusterNodes = riak_governor_util:get_cluster_nodes(),
    Ensembles = [ClusterNodes|Ensembles0],
    [ensure_ensemble_started(Nodes) || Nodes <- Ensembles].

ensure_ensemble_started(Nodes) ->
    ShouldStart    = ordsets:is_element(node(), Nodes),
    AlreadyStarted = ensemble_started(Nodes),
    ShouldStart andalso not AlreadyStarted andalso start_ensemble(Nodes),
    add_ensemble_to_index(Nodes).

%% Determine the set of ensembles. Currently, there is one ensemble of each
%% unique set of preflist owning nodes.
all_ensembles(Size) ->
    GroupF = fun(Prefs, Groups) ->
                     {_Indicies, Nodes} = lists:unzip(Prefs),
                     Key = lists:usort(Nodes),
                     sets:add_element(Key, Groups)
             end,
    {ok, Ring} = riak_core_ring_manager:get_my_ring(),
    AllPrefs = riak_core_ring:all_preflists(Ring, Size),
    PrefGroups = lists:foldl(GroupF, sets:new(), AllPrefs),
    Ensembles = sets:to_list(PrefGroups),
    lists:filter(fun(Nodes) -> length(Nodes) > 1 end, Ensembles).

add_ensemble_to_index(Nodes) ->
    ets:insert(?MODULE, {Nodes, riak_governor_util:ensemble_name(Nodes)}).

ensemble_started(Nodes) ->
    ets:lookup(?MODULE, Nodes) =/= [].

start_ensemble(Nodes) ->
    EnsembleName = riak_governor_util:ensemble_name(Nodes),
    Peers = lists:map(fun(Node) -> {EnsembleName, Node} end, Nodes),
    riak_governor_spi:start_ensemble(EnsembleName, Peers).
