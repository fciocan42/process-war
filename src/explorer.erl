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

handle_call(exploring, From, State = #state{status = ready}) ->
    {reply, {ok, exploring}, State#state{status = exploring}};

handle_call(exploring, From, State = #state{status=paused}) ->
    {reply, {ok, paused}, State};

handle_call(exploring, _From, State = #state{status = exploring}) ->
    {Reply, ReplyState} = case exploring(State) of
        {ok, NewState} -> {{ok, exploring}, NewState};
        {error, _Msg} -> {{ok, paused}, State#state{status = paused}}
    end,
    {reply, Reply, ReplyState}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.
 
 terminate(Reason, _State) ->
    io:format("Goodbye, brave warriors!~p~n",[Reason]),
    ok.


%%% HELPERS %%%

exploring(State) ->
    {Direction, CellState, Coords} = compute_next_move(State),
    case gs_war_map:move(Direction) of
        {ok, moved} ->
            {ok, update_state(State, Direction, CellState, Coords)};
        {error, Msg} ->
            {{error, Msg}, State}
    end.

update_state(State, Direction, CellState, {X, Y}) ->
    % TODO  Pick reward and add point only if the reward is available
    PrevMove = State#state.current_position,
    PointsNow = State#state.points,
    Points = case CellState of
        reward -> PointsNow + 1;
        _ -> PointsNow
    end,
    State#state{
        current_position = #coord{x = X, y = Y},
        current_move = Direction,
        previous_move = PrevMove,
        points = Points
    }.

available_steps() ->
    lisis:filter(
        fun({_Direction, CellState, _Coords}) -> CellState =/= out end, gs_war_map:available_steps()
    ).

opposite_directions(Dir1,  Dir2) ->
    case Dir1 of
        right -> Dir2 == left;
        left -> Dir2 == right;
        up -> Dir2 == down;
        down  -> Dir2 == up
    end.

compute_next_move(State)->
    Steps = available_steps(),
    % 1st priority: Go on rewards
    case lists:filter(fun ({_Direction, CellState, _Coords}) -> CellState == reward end, Steps) of
        [] ->
            % 2nd priority: Go in the same direction
            case lists:filter(fun ({Direction, _CellState, _Coords}) -> Direction == State#state.current_position end, Steps) of
                [] ->
                    % 3rd priority: Don’t go backwards
                    case lists:filter(fun (Direction, _CellState, _Coords) -> opposite_directions(Direction, State#state.current_position) == false end, Steps) of
                        [] ->
                            % Go backwards when it’s the only option
                            hd(Steps);
                        RandomSteps ->
                            % Last priority: choose random
                            ListSize = lists:length(RandomSteps),
                            lists:nth(rand:uniform(ListSize), RandomSteps)
                    end;
                [NextMove] ->
                    NextMove
            end;
        Rewards ->
            % 1.1 Go on a reward wich is in the curresnt direction if it exists
            case [{Direction, CellState, Coords} || {Direction, CellState, Coords} <- Rewards, Direction == State#state.current_position]  of
                [] ->
                    hd(Rewards);
                [Reward] ->
                    Reward
            end
    end.
