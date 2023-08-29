-module(incident_supervisor).
-behaviour(supervisor).
-define(SERVER, ?MODULE).

-export([start_link/0, start_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(IncidentId) ->
    ChildSpec = {IncidentId, {incident, start_link, [IncidentId]}, transient, 5000, worker, [incident]},
    supervisor:start_child(?SERVER, ChildSpec).

init([]) ->
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, []}}.

