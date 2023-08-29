-module(unit_manager).
-behaviour(gen_server).

%%% External API exports
-export([start_link/0, get_units/0, update_distance/2, get_preferred_unit/0, unit_availability_changed/2, assign_preferred_unit/0]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% API Functions
start_link() ->
    error_logger:info_msg("Starting unit_manager gen_server~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_units() ->
    gen_server:call(?MODULE, get_units).

get_preferred_unit() ->
    gen_server:call(?MODULE, get_preferred_unit).

assign_preferred_unit() ->
    gen_server:call(?MODULE, assign_preferred_unit).

unit_availability_changed(UnitPid, Availability) ->
    gen_server:cast(?MODULE, {availability_changed, UnitPid, Availability}).

update_distance(Pid, Distance) ->
    % error_logger:info_msg("Updating distance for unit with PID ~p to ~p~n", [Pid, Distance]),
    gen_server:cast(?MODULE, {update_distance, Pid, Distance}).

%%% gen_server callbacks 
init([]) ->
    error_logger:info_msg("Initializing unit_manager gen_server with 10 units~n"),
    Units = [start_unit() || _ <- lists:seq(1, 10)],
    {ok, Units}.

%%% synchronous callbacks
handle_call(get_preferred_unit, _From, State) ->
    AvailableUnits = lists:filter(fun ({unit, _, _, IsAvailable}) -> IsAvailable end, State),
    SortedUnits = lists:sort(fun compare_units_by_distance/2, AvailableUnits),
    PreferredUnit = case SortedUnits of
        [] -> 
            error_logger:info_msg("No units available~n"),
            undefined; % Return undefined if no units are available
        _ -> 
            error_logger:info_msg("Returning preferred unit~n"),
            hd(SortedUnits)
    end,
    {reply, PreferredUnit, State};

handle_call(assign_preferred_unit, _From, State) ->
    case get_preferred_unit(State) of
        {error, no_available_units} ->
            error_logger:info_msg("No units available to assign~n"),
            {reply, {error, no_available_units}, State};
        {ok, Pid, UpdatedState} ->
            %% Call the unit to set its availability to false
            error_logger:info_msg("Tell unit with PID ~p to update its status to false~n", [Pid]),
            unit:set_availability(Pid, false),
            {reply, {ok, Pid}, UpdatedState}
    end;

handle_call(get_units, _From, State) ->
    error_logger:info_msg("Get all units~n"),
    {reply, State, State}.

%%% asynchronous callbacks
handle_cast({update_distance, UnitPid, Distance}, State) ->
    % error_logger:info_msg("Updating distance for unit with PID ~p~n", [UnitPid]),
    UpdatedState = lists:map(fun
        ({unit, Pid, _, IsAvailable}) when Pid == UnitPid ->
            {unit, Pid, Distance, IsAvailable};
        (Other) -> Other
    end, State),
    {noreply, UpdatedState};

handle_cast({availability_changed, UnitPid, Availability}, State) ->
    error_logger:info_msg("Manager received availability update for PID ~p. New availability: ~p~n", [UnitPid, Availability]),
    UpdatedState = lists:map(fun
        ({unit, Pid, Distance, _} = _Unit) when Pid == UnitPid ->
            {unit, Pid, Distance, Availability};
        (Other) -> Other
    end, State),
    {noreply, UpdatedState}.

handle_info(_Info, State) ->
    error_logger:info_msg("Received unhandled info message in unit_manager gen_server~n"),
    {noreply, State}.

terminate(_Reason, _State) ->
    error_logger:info_msg("Terminating unit_manager gen_server~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    error_logger:info_msg("Code change in unit_manager gen_server~n"),
    {ok, State}.

%%% Private Functions
compare_units_by_distance({unit, _, Dist1, _}, {unit, _, Dist2, _}) ->
    Dist1 < Dist2.

get_preferred_unit(State) ->
    AvailableUnits = lists:filter(fun ({unit, _, _, IsAvailable}) -> IsAvailable end, State),
    SortedUnits = lists:sort(fun compare_units_by_distance/2, AvailableUnits),
    case SortedUnits of
        [] ->
            error_logger:info_msg("No available units found~n"),
            {error, no_available_units};
        [{unit, Pid, Distance, _IsAvailable} | Rest] ->
            UpdatedUnit = {unit, Pid, Distance, false},
            NewState = [UpdatedUnit | Rest] ++ lists:subtract(State, [hd(SortedUnits)]),
            error_logger:info_msg("Selected preferred unit with PID ~p~n", [Pid]),
            {ok, Pid, NewState}
    end.

random_call_sign() ->
    lists:concat([random_letter() || _ <- lists:seq(1, 5)]).

random_distance() ->
    rand:uniform(100).

random_letter() ->
    rand:uniform(26) + 96.

start_unit() ->
    CallSign = random_call_sign(),
    Distance = random_distance(),
    {ok, Pid} = unit:start_link(CallSign, Distance),
    {unit, Pid, Distance, true}. % unit is available by default