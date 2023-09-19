-module(g_counter).
-behaviour(gen_server).

%% API
-export([start_link/0, merge/2]).

%% GenServer callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

%% ---------------------------
%% Public API
%% ---------------------------

start_link() ->
    io:format("Started new process~n"),
    gen_server:start_link(?MODULE, [], []).

merge(Pid, OtherCounter) ->
    gen_server:cast(Pid, {merge, OtherCounter}).

%% ---------------------------
%% GenServer Callbacks
%% ---------------------------

init([]) ->
    schedule_random_counter_event(),
    schedule_unexpected_termination(),
    InitialState = #{self() => 0},
    {ok, InitialState}.

handle_call(_Request, _From, State) -> 
    {reply, ok, State}.

handle_cast({merge, OtherCounter}, Counter) ->
    MergedCounter = merge_maps(Counter, OtherCounter),
    LocalCount = maps:get(self(), Counter, 0),
    io:format("Local count is: ~p, total count is: ~p~n", [LocalCount, total_count(MergedCounter)]),
    {noreply, MergedCounter}.

handle_info(timeout, Counter) -> 
    NewCounter = increment_counter(Counter),
    g_counter_mgr:notify({incremented, self(), NewCounter}),
    schedule_random_counter_event(), 
    {noreply, NewCounter};

handle_info(terminate, _Counter) -> 
    io:format("~p is terminating~n", [self()]),
    {stop, normal, #{}}.

%% ---------------------------
%% Utility Functions
%% ---------------------------

increment_counter(Counter) ->
    MyPid = self(),
    NewValue = maps:get(MyPid, Counter, 0) + 1,
    Counter#{MyPid => NewValue}.

merge_maps(Map1, Map2) ->
    maps:fold(fun(K, V2, Acc) ->
        V1 = maps:get(K, Map1, 0),
        Acc#{K => max(V1, V2)}
    end, Map1, Map2).

total_count(Counter) ->
    maps:fold(fun(_K, V, Acc) -> V + Acc end, 0, Counter).

schedule_random_counter_event() ->
    Interval = rand:uniform(10000) + 5000,
    timer:send_after(Interval, timeout).

schedule_unexpected_termination() ->
    TerminationInterval = rand:uniform(30000) + 30000, 
    timer:send_after(TerminationInterval, terminate).
