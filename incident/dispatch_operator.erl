-module(dispatch_operator).
-behaviour(gen_server).

%%% External API exports
-export([start_link/0, report_incident/3, close_incident/1, get_active_incidents/0, crash_last/0]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {incidents = [], last_incident_id = 0}).

%%% API Functions
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

report_incident(Type, Description, Severity) ->
    gen_server:call(?SERVER, {report_incident, Type, Description, Severity}).

get_active_incidents() ->
    gen_server:call(?SERVER, get_active_incidents).

close_incident(IncidentPid) ->
    gen_server:cast(IncidentPid, external_close).

crash_last() ->
    gen_server:call(?SERVER, crash_last).

%%% gen_server callbacks

init([]) ->
    error_logger:info_msg("Initializing dispatch operator"),
    {ok, _} = dets:open_file(incident_table, [{file, "incident_table.dets"}]),
    {ok, #state{last_incident_id = 0}}.

%%% synchronous callbacks
handle_call({report_incident, Type, Description, Severity}, _From, State) ->
    NewIncidentId = State#state.last_incident_id + 1,
    IncidentData = {Type, Description, Severity, self(), undefined},
    ok = dets:insert(incident_table, {NewIncidentId, IncidentData}),

    error_logger:info_msg("Saving: ~p to persistent storage~n", [Description]),

    {ok, IncidentPid} = incident_supervisor:start_child(NewIncidentId),
    NewIncidents = [{IncidentPid, NewIncidentId} | State#state.incidents],
    {reply, {ok, IncidentPid}, State#state{incidents = NewIncidents, last_incident_id = NewIncidentId}};


handle_call(get_active_incidents, _From, State) ->
    {reply, State#state.incidents, State};

handle_call(crash_last, _From, State) ->
    case State#state.incidents of
        [] ->
            {reply, no_active_incidents, State};
        [{FirstIncidentPid, _IncidentId} | _Rest] ->
            incident:crash(FirstIncidentPid),
            {reply, ok, State}
    end.

%%% asynchronous callbacks
handle_cast({close_incident, IncidentPid}, State) ->
    error_logger:info_msg("Dispatch is terminating: ~p~n", [IncidentPid]),

    gen_server:cast(IncidentPid, external_close),
    NewIncidents = lists:delete(IncidentPid, State#state.incidents),
    {noreply, State#state{incidents = NewIncidents}}.

handle_info({incident_not_closed, Description, IncidentPid}, State) ->
    error_logger:info_msg("Spawining new process to talk to operator about: ~p~n", [Description]),
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
    ok = dets:close(incident_table),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.