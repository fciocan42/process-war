%%%-------------------------------------------------------------------
%% @doc process_war top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(process_war_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-define(SERVER, ?MODULE).
-define(WAR_MAP_SUP, war_map_sup).
-define(EXP_SUP, explorer_sup).

start_link(Config) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [Config]).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([Config]) ->
    SupFlags =
        #{strategy => one_for_all,
          intensity => 0,
          period => 1},
    ChildSpecs =[#{id => ?WAR_MAP_SUP,
           start => {?WAR_MAP_SUP, start_link, []},
           restart => permanent,
           shutdown => brutal_kill,
           type => worker,
           modules => [?WAR_MAP_SUP]},#{id => ?EXP_SUP,
           start => {?EXP_SUP, start_link, [Config]},
           restart => permanent,
           shutdown => brutal_kill,
           type => worker,
           modules => [?EXP_SUP]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
