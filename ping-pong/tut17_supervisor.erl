-module(tut17_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    PongSpec = {tut17_pong, {tut17_pong, start_link, []}, permanent, 5000, worker, [tut17_pong]},
    PingSpec = {tut17_ping, {tut17_ping, start_link, [tut17_pong]}, permanent, 5000, worker, [tut17_ping]},
    
    Children = [PongSpec, PingSpec],

    {ok, {{one_for_one, 5, 10}, Children}}.
