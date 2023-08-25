-module(incident).
-behaviour(gen_server).

%%% External API exports
-export([start_link/0, report/3, close/1]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Data record
-record(incident, {
    type,
    description,
    severity,
    operator_pid,
    unit_pid = undefined,  %% This will store the unit's PID
    timer = undefined     %% This stores the timer reference
}).

%%% API Functions
start_link() ->
    io:format("Starting incident gen_server~n"),
    gen_server:start_link(?MODULE, [], []).

report(Pid, {Type, Description, Severity}, OperatorPid) when Severity >= 1, Severity =< 5 ->
    io:format("Reporting an incident of type: ~s with description: ~s and severity: ~p~n", [Type, Description, Severity]),
    gen_server:call(Pid, {report, Type, Description, Severity, OperatorPid});

report(_, _, _) ->
    io:format("Invalid incident~n"),
    {error, invalid_severity}.

close(Pid) ->
    io:format("Closing incident ~p~n", [Pid]),
    gen_server:call(Pid, close).

%%% gen_server callbacks
init([]) ->
    io:format("Initializing incident gen_server~n"),
    {ok, #incident{}}.

handle_call({report, Type, Description, Severity, OperatorPid}, _From, _State) ->
    io:format("Handling report call in incident gen_server~n"),
    case unit_manager:assign_preferred_unit() of
        {error, no_available_units} ->
            io:format("No units available for the incident~n"),
            {reply, {error, no_available_units}, _State};
        {ok, UnitPid} ->
            io:format("Assigned unit with PID: ~p for the incident~n", [UnitPid]),
            TimerRef = erlang:send_after(40000, self(), {incident_not_resolved, OperatorPid}),
            {reply, ok, #incident{type = Type, 
                                  description = Description, 
                                  severity = Severity, 
                                  operator_pid = OperatorPid, 
                                  unit_pid = UnitPid,
                                  timer = TimerRef}}
    end;

handle_call(close, _From, State) ->
    io:format("Handling close call in incident gen_server~n"),
    erlang:cancel_timer(State#incident.timer),
    UnitPid = State#incident.unit_pid,
    case UnitPid of
        undefined -> ok;
        _ -> unit_manager:unit_availability_changed(UnitPid, true)
    end,
    {stop, normal, ok, State}.

handle_cast(close, State) ->
    io:format("Handling close cast message in incident gen_server~n"),
    erlang:cancel_timer(State#incident.timer),
    UnitPid = State#incident.unit_pid,
    case UnitPid of
        undefined -> ok;  %% No unit was assigned to this incident
        _ -> unit_manager:unit_availability_changed(UnitPid, true)
    end,
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({incident_not_resolved, OperatorPid}, State) ->
    io:format("Incident not resolved in time, sending a message to the operator~n"),
    OperatorPid ! {incident_not_closed, State#incident.description, self()},
    erlang:cancel_timer(State#incident.timer),
    NewTimerRef = erlang:send_after(40000, self(), {incident_not_resolved, OperatorPid}),
    {noreply, State#incident{timer = NewTimerRef}};

handle_info(_Info, State) ->
    io:format("Received unhandled info message in incident gen_server~n"),
    {noreply, State}.

terminate(normal, _State) ->
    io:format("Gracefully terminating incident gen_server~n"),
    ok;

terminate(_Reason, State) ->
    io:format("Terminating incident gen_server due to unexpected reason: ~p~n", [_Reason]),
    erlang:cancel_timer(State#incident.timer),
    UnitPid = State#incident.unit_pid,
    case UnitPid of
        undefined -> ok;  %% No unit was assigned to this incident
        _ -> unit_manager:unit_availability_changed(UnitPid, true)
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("Code change in incident gen_server~n"),
    {ok, State}.
