-module(unit).
-behaviour(gen_server).

%%% External API exports
-export([start_link/2, set_availability/2]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    id,
    manager,
    distance = rand:uniform(100), %% Random initial distance
    is_available = true           %% By default, the unit is available
}).

%%% API Functions
start_link(Id, Manager) ->
    gen_server:start_link(?MODULE, {Id, Manager}, []).

set_availability(Pid, Availability) when is_boolean(Availability) ->
    error_logger:info_msg("Setting availability for PID ~p to ~p~n", [Pid, Availability]),
    gen_server:cast(Pid, {set_availability, Availability}).

%%% gen_server callbacks
init({Id, Manager}) ->
    %% Start a timer to report its distance after a random duration.
    send_random_timer(),
    {ok, #state{id = Id, manager = Manager}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({set_availability, Availability}, State) ->
    %% Notify the manager about the change in availability
    error_logger:info_msg("Received availability update for PID ~p. New availability: ~p~n", [self(), Availability]),

    unit_manager:unit_availability_changed(self(), Availability),
    {noreply, State#state{is_available = Availability}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(report_distance, State = #state{is_available = true}) ->
    %% Only report distance if the unit is available
    NewDistance = rand:uniform(100),
    unit_manager:update_distance(self(), NewDistance),
    send_random_timer(),
    {noreply, State#state{distance = NewDistance}};

handle_info(report_distance, State) ->
    %% If the unit is not available, just reset the timer without reporting
    send_random_timer(),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Private Functions
send_random_timer() ->
    RandomTime = rand:uniform(7000) + 3000,
    erlang:send_after(RandomTime, self(), report_distance).
