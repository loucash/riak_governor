%% @doc This module defins a service provider behaviour enabling alternative
%% distributed consensus algorithm implementations to be provided to riak_governor
%%

-module(riak_governor_spi).

-export([get_leader/1]).
-export([start_ensemble/2]).

-callback get_leader(term()) -> {term(), node()}.
-callback start_ensemble(term(),list()) -> ok | already_started.

get_leader(Id) ->
    Provider = riak_governor_util:get_ensemble_provider(),
    get_leader(Provider,Id).

start_ensemble(Name,Peers) ->
    Provider = riak_governor_util:get_ensemble_provider(),
    start_ensemble(Provider,Name,Peers).

%%
%% internal
%%

get_leader(rafter,{Id,Node}) ->
    riak_governor_spi_rafter:get_leader({Id,Node});
get_leader(rafter,Id) ->
    riak_governor_spi_rafter:get_leader(Id);
get_leader(riak_ensemble,{Id,_Node}) ->
    riak_governor_spi_riak_ensemble:get_leader(Id);
get_leader(riak_ensemble,Id) ->
    riak_governor_spi_riak_ensemble:get_leader(Id);
get_leader(_Otherwise,_Id) ->
    throw(bad_ensemble_provider).

start_ensemble(rafter,Name,Peers) ->
    riak_governor_spi_rafter:start_ensemble(Name,Peers);
start_ensemble(riak_ensemble,Name,Peers) ->
    riak_governor_spi_riak_ensemble:start_ensemble(Name,Peers);
start_ensemble(_Otherwise,_Name,_Peers) ->
    throw(bad_ensemble_provider).



