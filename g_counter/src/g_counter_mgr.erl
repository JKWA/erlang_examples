-module(g_counter_mgr).
-behaviour(gen_server).

%% API
-export([start_link/0, notify/1]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% ---------------------------
%% Public API
%% ---------------------------

start_link() ->
    gen_server:start_link({local, g_counter_mgr}, ?MODULE, [], []).

notify(Msg) ->
    gen_server:cast(g_counter_mgr, {notify, Msg}).

%% ---------------------------
%% GenServer Callbacks
%% ---------------------------

init([]) ->
    Pids = lists:map(fun(_) -> spawn_and_monitor() end, lists:seq(1, 5)),
    {ok, Pids}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({notify, {incremented, _IncrementingPid, IncrementedState}}, Pids) ->
    io:format("Received incremented state: ~p~n", [IncrementedState]),
    lists:foreach(fun(Pid) -> g_counter:merge(Pid, IncrementedState) end, Pids),
    {noreply, Pids}.

handle_info({'DOWN', _Ref, process, DeadPid, _Reason}, Pids) ->
    io:format("Process ~p terminated. Restarting...~n", [DeadPid]),
    NewPid = spawn_and_monitor(),
    NewPids = lists:delete(DeadPid, Pids) ++ [NewPid], 
    {noreply, NewPids}.

%% ---------------------------
%% Utility Functions
%% ---------------------------

spawn_and_monitor() ->
    {ok, Pid} = g_counter:start_link(),
    erlang:monitor(process, Pid),
    Pid.
