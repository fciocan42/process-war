-module(explorer).

-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([start/1, pause/1, ready/1, get_neighbours/0]).

-include("records.hrl").

-type status() :: ready | exploring | paused.

-record(state, {
    current_position :: coord,
    current_move :: atom(),
    previous_move :: atom(),
    points :: integer(),
    status :: status(),
    name :: atom()
}).

start_link(Name) ->
    gen_server:start_link(Name, {local, ?MODULE}, ?MODULE, [Name], []).

init([Name]) ->
    {ok, #state{
        current_position = #coord{x = 0, y = 0},
        current_move = none,
        previous_move = none,
        points = 0,
        status = ready,
        name = Name 
    }}.

start(Name)->
    case gen_server:call(Name, exploring) of
        {ok, exploring} -> start(Name);
        {ok, paused} -> paused
    end.

pause(Name)->
    gen_server:call(Name, pause).

ready(Name)->
    gen_server:call(Name, ready).

get_neighbours()->
    {ok, Neighbours} = gen_server:call(gs_war_map, neighbours),
    Neighbours.

stop() ->
    gen_server:stop(?MODULE).


%%% CALLBACKS %%%

handle_call(pause, _From, State) ->
    NewState = State#state{status = paused},
    {reply, {ok, NewState}, NewState};

handle_call(ready, _From, State) ->
    NewState = State#state{status = ready},
    {reply, {ok, NewState}, NewState};

handle_call(exploring, From, State = #state{status=paused}) ->
    {reply, {ok, paused}, State};

handle_call(exploring, From, State = #state{status=exploring}) ->
    exploring(State),
    Reply = {ok, exploring},
    {reply, Reply, State};

handle_call(exploring, From, State)->
    {reply, {ok, exploring, "Already exploring!"}, State};

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.
 
 terminate(Reason, _State) ->
    io:format("Goodbye, brave warriors!~p~n",[Reason]),
    ok.


%%% HELPERS %%%

exploring(State) ->
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

get_neighbours()->
    gen_server:call(gs_war_map, neighbours).
