-module(explorer).

-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-include("records.hrl").

-type status() :: ready | exploring.

-record(state, {
    current_position :: coord,
    current_move :: atom(),
    previous_move :: atom(),
    points :: integer(),
    status :: status()
}).

start_link(Name) ->
    gen_server:start_link(Name, {local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
    {ok, #state{
        current_position = #coord{x = 0, y = 0},
        current_move = none,
        previous_move = none,
        points = 0,
        status = ready
    }}.

move()

handle_call(move, From, State) ->


handle_call(start, From, State) ->
    start_exploring(State),
    gen_server:call(?MODULE, )




start_exploring(State) ->
    {Direction, CellState, Coords} = compute_next_move(),
    case gs_war_map:move(Direction) of
        {ok, moved} ->
            update_state(State, Direction, Coords),
            start_exploring();
        {error, Msg} -> {error, Msg}
    end.

update_state() ->

available_steps() ->
    lisis:filter(
        fun({_Direction, CellState, _Coords}) -> CellState =/= out end, gs_war_map:available_steps()
    ).


compute_next_move()->
   Steps = available_steps(),
   ListSize = lists:length(Steps),
   lists:nth(rand:uniform(ListSize), Steps).
