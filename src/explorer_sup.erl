-module(explorer_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(WORKER_MOD, explorer).
-define(WORKER_NAME, explorer_1).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 1,
          period => 5},
    ChildSpecs =
        [#{id => ?WORKER_NAME,
           start => {?WORKER_MOD, start_link, [?WORKER_NAME]},
           restart => permanent,
           shutdown => brutal_kill,
           type => worker,
           modules => [?WORKER_MOD]}],
    {ok, {SupFlags, ChildSpecs}}.
