-module(unit_manager).
-behaviour(gen_server).

%%% External API exports
-export([start_link/0, get_units/0, update_distance/2, get_preferred_unit/0, unit_availability_changed/2, assign_preferred_unit/0]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% API Functions
start_link() ->
    io:format("Starting unit_manager gen_server~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_units() ->
    io:format("Fetching all units~n"),
    gen_server:call(?MODULE, get_units).

get_preferred_unit() ->
    io:format("Requesting preferred unit~n"),
    gen_server:call(?MODULE, get_preferred_unit).

assign_preferred_unit() ->
    io:format("Assigning preferred unit~n"),
    gen_server:call(?MODULE, assign_preferred_unit).

unit_availability_changed(UnitPid, Availability) ->
    io:format("Updating availability for unit with PID ~p to ~p~n", [UnitPid, Availability]),
    gen_server:cast(?MODULE, {availability_changed, UnitPid, Availability}).

update_distance(Pid, Distance) ->
    % io:format("Updating distance for unit with PID ~p to ~p~n", [Pid, Distance]),
    gen_server:cast(?MODULE, {update_distance, Pid, Distance}).

%%% gen_server callbacks 
init([]) ->
    io:format("Initializing unit_manager gen_server with 10 units~n"),
    Units = [start_unit() || _ <- lists:seq(1, 10)],
    {ok, Units}.

handle_call(get_preferred_unit, _From, State) ->
    io:format("Handling get_preferred_unit call~n"),
    AvailableUnits = lists:filter(fun ({unit, _, _, IsAvailable}) -> IsAvailable end, State),
    SortedUnits = lists:sort(fun compare_units_by_distance/2, AvailableUnits),
    PreferredUnit = case SortedUnits of
        [] -> 
            io:format("No units available~n"),
            undefined; % Return undefined if no units are available
        _ -> 
            io:format("Returning preferred unit~n"),
            hd(SortedUnits)
    end,
    {reply, PreferredUnit, State};

handle_call(assign_preferred_unit, _From, State) ->
    io:format("Handling assign_preferred_unit call~n"),
    case get_preferred_unit(State) of
        {error, no_available_units} ->
            io:format("No units available to assign~n"),
            {reply, {error, no_available_units}, State};
        {ok, Pid, UpdatedState} ->
            %% Call the unit to set its availability to false
            io:format("Setting availability to false for unit with PID ~p~n", [Pid]),
            unit:set_availability(Pid, false),
            {reply, {ok, Pid}, UpdatedState}
    end;

handle_call(get_units, _From, State) ->
    io:format("Handling get_units call~n"),
    {reply, State, State}.

handle_cast({update_distance, UnitPid, Distance}, State) ->
    % io:format("Updating distance for unit with PID ~p~n", [UnitPid]),
    UpdatedState = lists:map(fun
        ({unit, Pid, _, IsAvailable}) when Pid == UnitPid ->
            {unit, Pid, Distance, IsAvailable};
        (Other) -> Other
    end, State),
    {noreply, UpdatedState};

handle_cast({availability_changed, UnitPid, Availability}, State) ->
    io:format("Received availability update for PID ~p. New availability: ~p~n", [UnitPid, Availability]),
    UpdatedState = lists:map(fun
        ({unit, Pid, Distance, _} = _Unit) when Pid == UnitPid ->
            {unit, Pid, Distance, Availability};
        (Other) -> Other
    end, State),
    {noreply, UpdatedState}.

handle_info(_Info, State) ->
    io:format("Received unhandled info message in unit_manager gen_server~n"),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Terminating unit_manager gen_server~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    io:format("Code change in unit_manager gen_server~n"),
    {ok, State}.

%%% Private Functions
compare_units_by_distance({unit, _, Dist1, _}, {unit, _, Dist2, _}) ->
    Dist1 < Dist2.

get_preferred_unit(State) ->
    io:format("Fetching preferred unit~n"),
    AvailableUnits = lists:filter(fun ({unit, _, _, IsAvailable}) -> IsAvailable end, State),
    SortedUnits = lists:sort(fun compare_units_by_distance/2, AvailableUnits),
    case SortedUnits of
        [] ->
            io:format("No available units found~n"),
            {error, no_available_units};
        [{unit, Pid, Distance, _IsAvailable} | Rest] ->
            UpdatedUnit = {unit, Pid, Distance, false},
            NewState = [UpdatedUnit | Rest] ++ lists:subtract(State, [hd(SortedUnits)]),
            io:format("Selected preferred unit with PID ~p~n", [Pid]),
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
    {unit, Pid, Distance, true}. % true signifies that the unit is available by default