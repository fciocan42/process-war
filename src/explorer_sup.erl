-module(explorer_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(WORKER_MOD, explorer).
-define(WORKER_NAME_1, explorer_1).
-define(WORKER_NAME_2, explorer_2).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 1,
          period => 5},
    ChildSpecs =
        [#{id => ?WORKER_NAME_1,
           start => {?WORKER_MOD, start_link, [?WORKER_NAME_1]},
           restart => permanent,
           shutdown => brutal_kill,
           type => worker,
           modules => [?WORKER_MOD]},
           #{id => ?WORKER_NAME_2,
           start => {?WORKER_MOD, start_link, [?WORKER_NAME_2]},
           restart => permanent,
           shutdown => brutal_kill,
           type => worker,
           modules => [?WORKER_MOD]}],
    {ok, {SupFlags, ChildSpecs}}.
