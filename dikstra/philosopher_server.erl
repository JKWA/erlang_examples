-module(philosopher_server).
-behaviour(gen_server).

%% API
-export([start_link/3, start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(EAT_TIME, 1000).

start(Room) ->
    Forks = [fork_server:start_link() || _ <- lists:seq(1, 5)],
    [start_link(Room, Forks, I) || I <- lists:seq(1, 5)].


start_link(Room, Forks, Id) ->
    gen_server:start_link(?MODULE, {Room, Forks, Id}, []).

init({Room, Forks, Id}) ->
    enter_room(Room, Id, {Room, Forks, Id}),
    {ok, {Room, Forks, Id}}.

handle_cast({try_to_eat, LeftFork, RightFork, Id}, State) ->
    case fork_server:request(LeftFork) of
        {fork, _, ready} ->
            case fork_server:request(RightFork) of
                {fork, _, ready} ->
                    io:format("Philosopher ~p (~p) is eating.~n", [Id, self()]),
                    timer:sleep(?EAT_TIME),
                    fork_server:drop(LeftFork),
                    fork_server:drop(RightFork),
                    room_server:exit(self()),
                    io:format("Philosopher ~p (~p) has finished eating and is leaving the room.~n", [Id, self()]),
                    {noreply, State};
                _ ->
                    fork_server:drop(LeftFork),
                    {noreply, State}
            end;
        _ ->
            {noreply, State}
    end.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_info(timeout, State) ->
    {Room, _, Id} = State,
    enter_room(Room, Id, State),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

enter_room(Room, Id, State) ->
    case room_server:enter(Room) of
        allowed -> 
            {Forks, _} = State,
            LeftFork = lists:nth(Id, Forks),
            RightFork = lists:nth((Id rem 5) + 1, Forks),
            gen_server:cast(self(), {try_to_eat, LeftFork, RightFork, Id}),
            allowed;
        denied -> 
            timer:send_after(1000, timeout),
            denied
    end.

