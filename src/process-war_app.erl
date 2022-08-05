%%%-------------------------------------------------------------------
%% @doc process-war public API
%% @end
%%%-------------------------------------------------------------------

-module(process-war_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    process-war_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
