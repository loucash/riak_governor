-module(riak_governor_spi_rafter).
-behaviour(riak_governor_spi).

-include_lib("rafter/include/rafter_opts.hrl").

-export([get_leader/1]).
-export([start_ensemble/2]).

get_leader(Id) ->
    rafter:get_leader(Id).

start_ensemble(Name, Peers) ->
    % TODO: configurable logdir
    Me = {Name, node()},
    Opts = #rafter_opts{state_machine=rafter_backend_ets, logdir="./data"},
    case rafter:start_node(Me, Opts) of
        {ok, _} ->
            catch rafter:set_config(Name, Peers),
            ok;
        {error,{already_started,_}} ->
            ok
    end.

