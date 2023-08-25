-module(tut17).
-export([start/0, ping/2, pong/0, init/0]).
-export([start_supervisor/0, start_children/1]).

ping(0, Pong_PID) ->
    Pong_PID ! finished,
    io:format("ping finished~n", []);

ping(N, Pong_PID) ->
    Pong_PID ! {ping, self()},
    receive
        pong ->
            io:format("Ping received pong~n", [])
    end,
    ping(N-1, Pong_PID).

pong() ->
    receive
        finished ->
            io:format("Pong finished~n", []);
        {ping, Ping_PID} ->
            io:format("Pong received ping~n", []),
            Ping_PID ! pong,
            pong()
    end.

init() ->
    start_supervisor().

start() ->
    Pong_PID = spawn(tut17, pong, []),
    spawn(tut17, ping, [3, Pong_PID]).

start_supervisor() ->
    Supervisor = spawn_link(fun() -> start_children(some_argument) end),
    register(supervisor, Supervisor),
    Supervisor.

start_children(_Args) ->
    process_flag(trap_exit, true),
    Pong_PID = spawn_link(tut17, pong, []),
    spawn_link(tut17, ping, [3, Pong_PID]),
    supervise_children().

supervise_children() ->
    receive
        {'EXIT', _Child, _Reason} ->
            io:format("A child process died. Restarting...~n", []),
            start_children(1)
    end.
