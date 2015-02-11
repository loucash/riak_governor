-module(riak_governor_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    case riak_governor_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core_ring_events:add_guarded_handler(
                   riak_governor_ring_handler, []),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
