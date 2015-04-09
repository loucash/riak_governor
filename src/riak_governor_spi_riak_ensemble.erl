-module(riak_governor_spi_riak_ensemble).
-behaviour(riak_governor_spi).

-define(ENSEMBLE_BACKEND, riak_ensemble_basic_backend).

-export([get_leader/1]).
-export([start_ensemble/2]).
-export([stop_ensemble/1]).

get_leader({Id, _Node}) ->
    riak_ensemble_manager:get_leader(Id).

start_ensemble(Name,Peers) ->
    % since ensemble creation might return failure/timeout and YET actually succeed,
    % hence check ensemble isn't created already before recreating
    case riak_ensemble_manager:known_ensembles() of
        {ok, Known} ->
            case proplists:is_defined(Name, Known) of
                true -> ok;
                false -> riak_ensemble_manager:create_ensemble(Name, {Name,node()}, Peers, ?ENSEMBLE_BACKEND, [])
            end;
        Other ->
            Other
    end.

stop_ensemble(_Name) ->
    ok.
