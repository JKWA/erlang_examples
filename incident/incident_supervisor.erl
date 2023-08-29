-module(incident_supervisor).
-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(IncidentId) ->
    ChildSpec = {IncidentId, {incident, start_link, [IncidentId]}, transient, 5000, worker, [incident]},
    supervisor:start_child(?MODULE, ChildSpec).

init([]) ->
    RestartStrategy = {one_for_one, 5, 10},
    {ok, {RestartStrategy, []}}.

