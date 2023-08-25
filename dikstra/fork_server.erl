-module(fork_server).
-behaviour(gen_server).

-export([start_link/1, init/1, request/1, drop/1, handle_call/3]).

start_link(Id) ->
    gen_server:start_link(?MODULE, Id, []).

init(Id) ->
    {ok, Id}.

handle_call(request, _From, Id) ->
    {reply, {fork, Id, ready}, Id};
handle_call(drop, _From, Id) ->
    {reply, ok, Id}.

request(Fork) ->
    gen_server:call(Fork, request).

drop(Fork) ->
    gen_server:call(Fork, drop).
