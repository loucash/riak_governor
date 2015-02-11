-module(riak_governor_spi_riak_ensemble).
-behaviour(riak_governor_spi).

-define(ENSEMBLE_BACKEND, riak_ensemble_basic_backend).

-export([get_leader/1]).
-export([start_ensemble/2]).

get_leader(Id) ->
    riak_ensemble_manager:get_leader(Id).

start_ensemble(Name,Peers) ->
    riak_ensemble_manager:create_ensemble(Name, {Name,node()}, Peers, ?ENSEMBLE_BACKEND, []),
    ok.

