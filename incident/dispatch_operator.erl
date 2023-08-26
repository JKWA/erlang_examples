-module(dispatch_operator).
-behaviour(gen_server).

%%% External API exports
-export([start_link/0, report_incident/3, close_incident/1, get_active_incidents/0]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {incidents = []}).

%%% API Functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

report_incident(Type, Description, Severity) ->
    gen_server:cast(?SERVER, {report_incident, Type, Description, Severity}).

get_active_incidents() ->
    gen_server:call(?SERVER, get_active_incidents).

close_incident(IncidentPid) ->
    gen_server:cast(IncidentPid, external_close).

%%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call(get_active_incidents, _From, State) ->
    {reply, State#state.incidents, State}.

handle_cast({report_incident, Type, Description, Severity}, State) ->
    {ok, IncidentPid} = incident:start_link(),
    incident:report(IncidentPid, {Type, Description, Severity}, self()),
    {noreply, State#state{incidents = [IncidentPid | State#state.incidents]}};

handle_cast({close_incident, IncidentPid}, State) ->
    gen_server:cast(IncidentPid, external_close),
    NewIncidents = lists:delete(IncidentPid, State#state.incidents),
    {noreply, State#state{incidents = NewIncidents}}.

handle_info({incident_not_closed, Description, IncidentPid}, State) ->
    spawn(fun() -> handle_user_input(Description, IncidentPid) end),
    {noreply, State};

handle_info(Msg, State) ->
    error_logger:error_msg("Unhandled message: ~p~n", [Msg]),
    {noreply, State}.

%%% Private Functions
handle_user_input(Description, IncidentPid) ->
    io:format("Alert: Incident '~s' has not been resolved yet!~n", [Description]),
    io:format("Do you want to close this incident? (y/n) ~p: ", [IncidentPid]),
    UserInput = io:get_line(""),
    case string:lowercase(UserInput) of
        "y\n" -> 
            gen_server:cast(?SERVER, {close_incident, IncidentPid});
        "n\n" -> 
            ok;
        _ -> 
            io:format("Invalid input. Please enter 'y' or 'n'.~n")
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
