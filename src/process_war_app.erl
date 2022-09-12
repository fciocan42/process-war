%%%-------------------------------------------------------------------
%% @doc process_war public API
%% @end
%%%-------------------------------------------------------------------

-module(process_war_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    process_war_sup:start_link(),
    explorer_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
