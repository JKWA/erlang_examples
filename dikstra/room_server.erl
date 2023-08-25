-module(room_server).
-behaviour(gen_server).

%% API
-export([start_link/0, enter/1, exit/1]).

%% gen_server callbacks
-export([init/1, handle_call/3]).

-define(MAX_OCCUPANCY, 3).

start_link() ->
    gen_server:start_link(?MODULE, 0, []).

init(InitialOccupancy) ->
    {ok, InitialOccupancy}.

enter(Room) ->
    gen_server:call(Room, {enter, self()}).

exit(Room) ->
    gen_server:call(Room, {exit, self()}).

handle_call({enter, _Philosopher}, _From, Occupancy) when Occupancy < ?MAX_OCCUPANCY ->
    {reply, allowed, Occupancy + 1};
handle_call({enter, _Philosopher}, _From, Occupancy) ->
    {reply, denied, Occupancy};
handle_call({exit, _Philosopher}, _From, Occupancy) when Occupancy > 0 ->
    {reply, ok, Occupancy - 1};
handle_call(_Request, _From, State) ->
    {reply, error, State}.
