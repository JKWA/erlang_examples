-module(incident_supervisor).
-behaviour(supervisor).

-export([start_link/0, start_child/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child() ->
    supervisor:start_child(?MODULE, []).

init([]) ->
    RestartStrategy = {simple_one_for_one, 0, 1},
    ChildSpec = {incident, {incident, start_link, []}, temporary, 5000, worker, [incident]},
    {ok, {RestartStrategy, [ChildSpec]}}.
