-module(riak_governor_ensemble_master).
-behaviour(gen_server).

%% API
-export([ring_changed/0]).
-export([ensure_ensembles/0]).
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
          ringhash      :: binary(),
          retry_backoff :: backoff:backoff()
         }).

-define(RETRY_DELAY_MIN, 100).
-define(RETRY_DELAY_MAX, 10000).

%%%===================================================================
%%% API
%%%===================================================================
ring_changed() ->
    gen_server:cast(?MODULE, ring_changed).

ensure_ensembles() ->
    gen_server:cast(?MODULE, ensure_ensembles).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    EnsembleSize = riak_governor_util:get_ensemble_size(),
    RingHash = get_ring_hash(),
    _ = ets:new(?MODULE, [named_table, public]),
    timer:apply_after(?RETRY_DELAY_MIN, ?MODULE, ensure_ensembles, []),
    RetryDelayMin = riak_governor:get_env(ensemble_creation_retry_delay_min, ?RETRY_DELAY_MIN),
    RetryDelayMax = riak_governor:get_env(ensemble_creation_retry_delay_max, ?RETRY_DELAY_MAX),
    {ok, #state{ensemble_size=EnsembleSize,
                ringhash=RingHash,
                retry_backoff=backoff:init(RetryDelayMin, RetryDelayMax)}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(ring_changed, #state{ringhash=RingHash}=State) ->
    case get_ring_hash() of
        RingHash ->
            {noreply, State};
        NewRingHash ->
            State2 = try_start_ensembles(State),
            {noreply, State2#state{ringhash=NewRingHash}}
    end;
handle_cast(ensure_ensembles, State) ->
    State2 = try_start_ensembles(State),
    {noreply, State2}.

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

try_start_ensembles(#state{retry_backoff=Backoff}=State) ->
    case ensure_ensembles_started(State) of
        ok ->
            {_, Backoff2} = backoff:succeed(Backoff),
            State#state{retry_backoff=Backoff2};
        {error, Error} ->
            % schedule another ensemble start attempt
            {RetryDelay, Backoff2} = backoff:fail(Backoff),
            lager:debug("Failed to create ensembles due to: ~p, will retry in ~bms", [Error, RetryDelay]),
            timer:apply_after(RetryDelay, ?MODULE, ensure_ensembles, []),
            State#state{retry_backoff=Backoff2}
    end.

ensure_ensembles_started(#state{ensemble_size=EnsembleSize}) ->
    Ensembles0 = all_ensembles(EnsembleSize),
    ClusterNodes = riak_governor_util:get_cluster_nodes(),
    Ensembles = [ClusterNodes|Ensembles0],
    Res = [ensure_ensemble_started(Nodes) || Nodes <- Ensembles,
                                             is_local_ensemble(Nodes),
                                             not ensemble_started(Nodes)],
    % validate all results are ok
    case lists:usort(Res) of
        [] -> ok;
        [ok] -> ok;
        _ -> {error, {unexpected_ensemble_start_results, Res}}
    end.

ensure_ensemble_started([_]) ->
    % do not create an ensemble for group of one
    ok;
ensure_ensemble_started(Nodes) ->
    case start_ensemble(Nodes) of
        ok ->
            add_ensemble_to_index(Nodes);
        Other ->
            Other
    end.

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
    sets:to_list(PrefGroups).

add_ensemble_to_index(Nodes) ->
    true = ets:insert(?MODULE, {Nodes, riak_governor_util:ensemble_name(Nodes)}),
    ok.

ensemble_started(Nodes) ->
    ets:lookup(?MODULE, Nodes) =/= [].

is_local_ensemble(Nodes) ->
    ordsets:is_element(node(), Nodes).

start_ensemble(Nodes) ->
    EnsembleName = riak_governor_util:ensemble_name(Nodes),
    Peers = lists:map(fun(Node) -> {EnsembleName, Node} end, Nodes),
    riak_governor_spi:start_ensemble(EnsembleName, Peers).
