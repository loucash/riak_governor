-module(riak_rafter_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

start(_StartType, _StartArgs) ->
    case riak_rafter_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core_ring_events:add_guarded_handler(
                   riak_rafter_ring_handler, []),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
