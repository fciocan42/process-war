%%%-------------------------------------------------------------------
%% @doc process_war public API
%% @end
%%%-------------------------------------------------------------------

-module(process_war_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, [#{explorer_num := ExpNum}]) ->
    process_war_sup:start_link(ExpNum).

stop(_State) ->
    ok.

%% internal functions