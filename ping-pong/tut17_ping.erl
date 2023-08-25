-module(tut17_ping).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link(Pong_PID) ->
    gen_server:start_link(?MODULE, [Pong_PID], []).

init([Pong_PID]) ->
    Pong_PID ! {ping, self()},
    {ok, {Pong_PID, 3}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(pong, {Pong_PID, N}) when N > 1 ->
    io:format("Ping received pong~n", []),
    Pong_PID ! {ping, self()},
    {noreply, {Pong_PID, N - 1}};
handle_info(pong, {Pong_PID, 1}) ->
    io:format("Ping received pong~n", []),
    Pong_PID ! finished,
    io:format("ping finished~n", []),
    {stop, normal, {Pong_PID, 0}};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
