%%%-------------------------------------------------------------------
%% @doc g_counter public API
%% @end
%%%-------------------------------------------------------------------

-module(g_counter_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    g_counter_mgr:start_link().

stop(_State) ->
    ok.
