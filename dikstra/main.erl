-module(main).
-export([start/0, stop/0]).

start() ->
    Forks = [fork:start_link() || _ <- lists:seq(1, 5)],
    PhilosopherPids = [philosopher:start_link(lists:nth(I, Forks), lists:nth(I rem 5 + 1, Forks)) || I <- lists:seq(1, 5)],
    application:set_env(main, pids, Forks ++ PhilosopherPids).

stop() ->
    {ok, Pids} = application:get_env(main, pids),
    lists:foreach(fun(Pid) -> Pid ! stop end, Pids).
