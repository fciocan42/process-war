-module(explorer).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-include("records.hrl").

-record(state, {
    current_position :: coord,
    current_move :: atom(),
    previous_move :: atom(),
    points :: integer()
}).

start_link(Name) ->
    gen_server:start_link(Name, {local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{
        current_position = #coord{x = 0, y = 0},
        current_move = none,
        previous_move = none,
        points = 0
    }}.


