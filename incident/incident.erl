-module(incident).
-behaviour(gen_server).

%%% External API exports
-export([start_link/1, close/1, crash/1]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Data record
-record(incident, {
    incident_id,
    type,
    description,
    severity,
    operator_pid,
    unit_pid = undefined,
    timer = undefined
}).

%%% API Functions
start_link(IncidentId) ->
    error_logger:info_msg("Starting gen_server for incident~n"),
    gen_server:start_link(?MODULE, IncidentId, []).

close(Pid) ->
    error_logger:info_msg("Closing incident with PID ~p~n", [Pid]),
    gen_server:cast(Pid, external_close).

crash(Pid) ->
    gen_server:call(Pid, crash).

%%% gen_server callbacks
init(IncidentId) ->
    error_logger:info_msg("Initializing incident~n"),
    case get_incident_data(IncidentId) of
        {error, no_data_found} = Error -> 
            {stop, Error};
        {ok, {Type, Description, Severity, OperatorPid, UnitPid}} -> 
            initialize_incident(IncidentId, {Type, Description, Severity, OperatorPid}, UnitPid)
    end.


%%% synchronous callback
handle_cast(external_close, State) ->
    error_logger:info_msg("External close request received~n"),
    NewState = internal_close(State),
    {noreply, NewState}.

%%% asynchronous callbacks
handle_call(crash, _From, _State) ->
    error_logger:error_msg("Crashing incident gen_server~n"),
    {stop, crash, crash, _State};

handle_call(_Request, _From, State) ->
    error_logger:error_msg("Received unexpected call in incident gen_server~n"),
    {reply, unexpected_call, State}.

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
get_incident_data(IncidentId) ->
    case dets:lookup(incident_table, IncidentId) of
        [] -> {error, no_data_found};
        [{_IncidentId, IncidentData}] -> {ok, IncidentData}
    end.

initialize_incident(IncidentId, {Type, Description, Severity, OperatorPid}, UnitPid) ->
    case UnitPid of
        undefined ->
            case unit_manager:assign_preferred_unit() of
                {error, no_available_units} = Error -> 
                    error_logger:warning_msg("No units available for the incident~n"),
                    {stop, Error};
                {ok, AssignedUnitPid} -> 
                    error_logger:info_msg("Assigned unit with PID: ~p for the incident~n", [AssignedUnitPid]),
                    TimerRef = erlang:send_after(40000, self(), {incident_not_resolved, OperatorPid}),
                    % Save the assigned UnitPid back to the database
                    ok = dets:insert(incident_table, {IncidentId, {Type, Description, Severity, OperatorPid, AssignedUnitPid}}),
                    {ok, #incident{incident_id = IncidentId,
                                   type = Type,
                                   description = Description,
                                   severity = Severity,
                                   operator_pid = OperatorPid,
                                   unit_pid = AssignedUnitPid,
                                   timer = TimerRef}}
            end;
        _ ->
            error_logger:info_msg("Rebooting ~p~n", [Description]),
            TimerRef = erlang:send_after(40000, self(), {incident_not_resolved, OperatorPid}),
            {ok, #incident{incident_id = IncidentId,
                           type = Type,
                           description = Description,
                           severity = Severity,
                           operator_pid = OperatorPid,
                           unit_pid = UnitPid,
                           timer = TimerRef}}
    end.




% initialize_incident(IncidentId, {Type, Description, Severity, OperatorPid}) ->
%     case unit_manager:assign_preferred_unit() of
%         {error, no_available_units} = Error -> 
%             error_logger:warning_msg("No units available for the incident~n"),
%             {stop, Error};
%         {ok, UnitPid} -> 
%             error_logger:info_msg("Assigned unit with PID: ~p for the incident~n", [UnitPid]),
%             TimerRef = erlang:send_after(40000, self(), {incident_not_resolved, OperatorPid}),
%             {ok, #incident{incident_id = IncidentId,
%                            type = Type,
%                            description = Description,
%                            severity = Severity,
%                            operator_pid = OperatorPid,
%                            unit_pid = UnitPid,
%                            timer = TimerRef}}
%     end.

% initialize_incident(IncidentId, {Type, Description, Severity, OperatorPid, UnitPid}) ->
%     case UnitPid of
%         undefined ->
%             % If it's a new incident, assign a preferred unit and start a timer.
%             case unit_manager:assign_preferred_unit() of
%                 {error, no_available_units} = Error -> 
%                     error_logger:warning_msg("No units available for the incident~n"),
%                     {stop, Error};
%                 {ok, NewUnitPid} -> 
%                     error_logger:info_msg("Assigned unit with PID: ~p for the incident~n", [NewUnitPid]),
%                     TimerRef = erlang:send_after(40000, self(), {incident_not_resolved, OperatorPid}),
%                     {ok, #incident{incident_id = IncidentId,
%                                    type = Type,
%                                    description = Description,
%                                    severity = Severity,
%                                    operator_pid = OperatorPid,
%                                    unit_pid = NewUnitPid,
%                                    timer = TimerRef}}
%             end;
%         _ ->
%             error_logger:info_msg("Incident ~p is being recovered~n", [Description]),
%             {ok, #incident{incident_id = IncidentId,
%                            type = Type,
%                            description = Description,
%                            severity = Severity,
%                            operator_pid = OperatorPid,
%                            unit_pid = UnitPid}}
%     end.

internal_close(State) ->
    error_logger:info_msg("Internally closing incident~n"),
    erlang:cancel_timer(State#incident.timer),
    UnitPid = State#incident.unit_pid,
    case UnitPid of
        undefined -> ok;
        _ -> unit_manager:unit_availability_changed(UnitPid, true)
    end,
    State.
