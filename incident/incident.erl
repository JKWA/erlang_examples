-module(incident).
-behaviour(gen_server).

%%% External API exports
-export([start_link/0, report/3, close/0]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Data record
-record(incident, {
    type,
    description,
    severity,
    operator_pid,
    unit_pid = undefined,
    timer = undefined
}).

%%% API Functions
start_link() ->
    error_logger:info_msg("Starting incident gen_server~n"),
    gen_server:start_link(?MODULE, [], []).

report(Pid, {Type, Description, Severity}, OperatorPid) when Severity >= 1, Severity =< 5 ->
    error_logger:info_msg("Reporting an incident of type: ~s with description: ~s and severity: ~p~n", [Type, Description, Severity]),
    gen_server:call(Pid, {report, Type, Description, Severity, OperatorPid}).

close() ->
    error_logger:info_msg("Closing current incident~n"),
    gen_server:cast(self(), external_close).

%%% gen_server callbacks
init([]) ->
    error_logger:info_msg("Initializing incident gen_server~n"),
    {ok, #incident{}}.

handle_call({report, Type, Description, Severity, OperatorPid}, _From, _State) ->
    error_logger:info_msg("Handling report call in incident gen_server~n"),
    case unit_manager:assign_preferred_unit() of
        {error, no_available_units} ->
            error_logger:warning_msg("No units available for the incident~n"),
            {reply, {error, no_available_units}, _State};
        {ok, UnitPid} ->
            error_logger:info_msg("Assigned unit with PID: ~p for the incident~n", [UnitPid]),
            TimerRef = erlang:send_after(40000, self(), {incident_not_resolved, OperatorPid}),
            {reply, ok, #incident{type = Type, 
                                  description = Description, 
                                  severity = Severity, 
                                  operator_pid = OperatorPid, 
                                  unit_pid = UnitPid,
                                  timer = TimerRef}}
    end.

handle_cast(external_close, State) ->
    error_logger:info_msg("External close request received in incident gen_server~n"),
    NewState = internal_close(State),
    {noreply, NewState}.

handle_info({incident_not_resolved, OperatorPid}, State) ->
    error_logger:warning_msg("Incident not resolved in time, notifying operator~n"),
    OperatorPid ! {incident_not_closed, State#incident.description, self()},
    erlang:cancel_timer(State#incident.timer),
    NewTimerRef = erlang:send_after(40000, self(), {incident_not_resolved, OperatorPid}),
    {noreply, State#incident{timer = NewTimerRef}};

handle_info(_Info, State) ->
    error_logger:error_msg("Received unhandled info message in incident gen_server~n"),
    {noreply, State}.

terminate(normal, _State) ->
    error_logger:info_msg("Gracefully terminating incident gen_server~n"),
    ok;

terminate(_Reason, State) ->
    error_logger:error_msg("Terminating incident gen_server due to unexpected reason~n"),
    erlang:cancel_timer(State#incident.timer),
    UnitPid = State#incident.unit_pid,
    case UnitPid of
        undefined -> ok;
        _ -> unit_manager:unit_availability_changed(UnitPid, true)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    error_logger:info_msg("Code change in incident gen_server~n"),
    {ok, State}.

%%% Private functions
internal_close(State) ->
    error_logger:info_msg("Internally closing incident~n"),
    erlang:cancel_timer(State#incident.timer),
    UnitPid = State#incident.unit_pid,
    case UnitPid of
        undefined -> ok;
        _ -> unit_manager:unit_availability_changed(UnitPid, true)
    end,
    State.
