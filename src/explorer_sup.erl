-module(explorer_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(WORKER_MOD, explorer).

start_link(ExpNum) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [ExpNum]).

init([ExpNum]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 1,
          period => 5},
    ChildSpecs =
        lists:map(fun(Num) ->
                     ExpName =
                         list_to_atom(atom_to_list(?WORKER_MOD) ++ "_" ++ integer_to_list(Num)),
                     #{id => ExpName,
                       start => {?WORKER_MOD, start_link, [ExpName]},
                       restart => permanent,
                       shutdown => brutal_kill,
                       type => worker,
                       modules => [?WORKER_MOD]}
                  end,
                  lists:seq(1, ExpNum)),
    {ok, {SupFlags, ChildSpecs}}.
