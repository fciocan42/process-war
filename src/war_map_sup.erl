-module(war_map_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(WORKER_MOD, gs_war_map).
-define(WORKER_NAME, war_map).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 1,
          period => 5},
    ChildSpecs =
        [#{id => ?WORKER_NAME,
           start => {?WORKER_MOD, start_link, []},
           restart => permanent,
           shutdown => brutal_kill,
           type => worker,
           modules => [?WORKER_MOD]}],
    {ok, {SupFlags, ChildSpecs}}.
