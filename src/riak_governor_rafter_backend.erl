-module(riak_governor_rafter_backend).

-behaviour(rafter_backend).

%% Rafter backend callbacks
-export([init/1, read/2, write/2]).

init(_) ->
    ok.

read(ping, State) ->
    {pong, State};
read(_Command, State) ->
    {ok, State}.

write(_Command, State) ->
    {ok, State}.
