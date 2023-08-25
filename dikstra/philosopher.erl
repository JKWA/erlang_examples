-module(philosopher).
-export([start_link/2]).

start_link(LeftFork, RightFork) ->
    random:seed(erlang:now()),
    spawn_link(fun() -> loop(LeftFork, RightFork) end).

loop(LeftFork, RightFork) ->
    think(),
    decide(LeftFork, RightFork).

decide(LeftFork, RightFork) ->
    case random:uniform(2) of
        1 -> eat(LeftFork, RightFork), loop(LeftFork, RightFork);
        2 -> think(), loop(LeftFork, RightFork)
    end.

eat(LeftFork, RightFork) ->
    LeftFork ! {request, self()},
    receive
        {fork, LeftFork, ready} ->
            RightFork ! {request, self()},
            receive
                {fork, RightFork, ready} ->
                    io:format("~p is eating~n", [self()]),
                    timer:sleep(random:uniform(2000)), %% random eat time up to 2 seconds
                    LeftFork ! drop,
                    RightFork ! drop;
                _ ->
                    LeftFork ! drop,
                    timer:sleep(random:uniform(2000))  % random wait time up to 2 seconds before trying again
            end;
        _ -> ok
    end.

think() ->
    io:format("~p is thinking~n", [self()]),
    timer:sleep(random:uniform(3000)).  %% random think time up to 3 seconds
