-module(fork).
-export([start_link/0, request/1, drop/1]).

start_link() ->
    spawn_link(fun loop/0).

request(Fork) ->
    Fork ! {request, self()}.

drop(Fork) ->
    Fork ! drop.

loop() ->
    receive
        {request, Philosopher} ->
            Philosopher ! {fork, self(), ready},
            wait_for_drop();
        stop ->
            exit(normal);
        _ -> loop()
    end.

wait_for_drop() ->
    receive
        drop -> loop();
        stop ->
            exit(normal)
    end.