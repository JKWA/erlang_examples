-module(calculator).
-export([start/0, loop/0, generate_adds/1, add/1]).

%% Constants
-define(TABLE, ref_to_numbers).
-define(MAX_RETRIES, 3).

%% Start & Main Loop Functions
start() ->
    case whereis(calc) of
        undefined ->
            spawn_and_register();
        OtherPid ->
            OtherPid ! terminate,
            % Add a small sleep to give the old process a chance to terminate
            timer:sleep(100),
            spawn_and_register()
    end.

spawn_and_register() ->
    Pid = spawn(fun loop/0),
    register(calc, Pid),
    ets:new(?TABLE, [named_table, public, {read_concurrency, true}]),
    Pid.

loop() ->
    receive
        terminate ->
            case ets:info(?TABLE) of
                undefined -> ok;  
                _ -> ets:delete(?TABLE)
            end,
            exit(normal);
        Other ->
            io:format("Unknown command received ~p~n", [Other]),
            loop()
    end.

%% Add & Retry Logic Functions
add(Numbers) -> 
    add(Numbers, 0, make_ref()).

add(Numbers, Retries, Ref) when Retries < ?MAX_RETRIES ->
    ets:insert(?TABLE, {Ref, Numbers}),
    Pid = sum_list:sum(Numbers, self(), Ref),
    MonitorRef = monitor(process, Pid),

    receive
        {result, Ref, Result} ->
            io:format("Sum of ~w is ~p~n", [lookup_numbers(Ref), Result]),
            demonitor(MonitorRef, [flush]);
        {'DOWN', MonitorRef, process, Pid, {error, known_crash}} ->
            io:format("The sum_list process for ~w had known crash. Retrying...~n", [lookup_numbers(Ref)]),
            demonitor(MonitorRef, [flush]),
            ets:delete(?TABLE, Ref),
            add(Numbers, Retries + 1, make_ref());
        {'DOWN', MonitorRef, process, Pid, _OtherReason} ->
            io:format("The sum_list process for ~w had unknown crash~n", [lookup_numbers(Ref)]),
            ets:delete(?TABLE, Ref)
    after 2000 -> % 2 seconds timeout
        io:format("Operation timed out for ~w. Retrying...~n", [lookup_numbers(Ref)]),
        demonitor(MonitorRef, [flush]),
        ets:delete(?TABLE, Ref),
        add(Numbers, Retries + 1, make_ref())
    end;

add(Numbers, _, _) ->
    io:format("Max retries reached for ~w. Operation aborted.~n", [Numbers]).

%% Utility Functions
lookup_numbers(Ref) ->
    case ets:lookup(?TABLE, Ref) of
        [{_, StoredNumbers}] when is_list(StoredNumbers) -> StoredNumbers;
        _ -> "Missing value" 
    end.

generate_adds(0) -> 
    ok;

generate_adds(N) when N > 0 ->
    RandomList = [rand:uniform(100) || _ <- lists:seq(1, rand:uniform(10))],
    io:format("Spawn #~p numbers ~w~n", [N, RandomList]),
    spawn(fun() -> add(RandomList) end),
    generate_adds(N - 1).
