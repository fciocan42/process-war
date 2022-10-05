-module(explorer).

-behaviour(gen_server).

-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([start/1, pause/1, ready/1, get_neighbours/0]).

-include("records.hrl").

-type status() :: ready | exploring | paused | targeting.

-record(state,
        {current_position :: coord,
         current_move :: atom(),
         previous_move :: atom(),
         points :: integer(),
         status :: status(),
         name :: atom(),
         new_msg_alert :: boolean(),
         msg_queue :: [{integer(), coord}],
         neighbours :: [term()],
         targeting_mode :: atom(),
         target_position :: coord}).

start_link(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, [Name], []).

init([Name]) ->
    init([Name, {0, 0}]);
init([Name, {X, Y}]) ->
    case gs_war_map:add_pid(X, Y) of
        {ok, _} ->
            {ok,
             #state{current_position = #coord{x = X, y = Y},
                    current_move = none,
                    previous_move = none,
                    points = 0,
                    status = ready,
                    name = Name,
                    msg_queue = [],
                    targeting_mode = application:get_env(proces_war, targeting_mode, focus)}};
        {error, Msg} ->
            {stop, Msg}
    end.

start(Name) ->
    case gen_server:call(Name, exploring) of
        {ok, exploring} ->
            start(Name);
        {ok, paused} ->
            paused
    end.

pause(Name) ->
    gen_server:call(Name, pause).

ready(Name) ->
    gen_server:call(Name, ready).

targeting(Name, MsgQueue) ->
    gen_server:call(Name, {targeting, MsgQueue}).

get_neighbours() ->
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
handle_call(exploring, _From, State = #state{status = ready}) ->
    {reply, {ok, exploring}, State#state{status = exploring}};
handle_call(exploring, _From, State = #state{status = paused}) ->
    {reply, {ok, paused}, State};
handle_call(exploring, _From, State = #state{status = exploring}) ->
    {Reply, ReplyState} =
        case exploring(State) of
            {ok, NewState, reward} ->
                Coords = NewState#state.current_position,
                Neighbours = NewState#state.neighbours,
                Points = explorer_util:earn_reward_and_inform(Coords, Neighbours),
                CurrentPoints = NewState#state.points,
                {{ok, exploring}, NewState#state{points = CurrentPoints + Points}};
            {ok, NewState, _} ->
                {{ok, exploring}, NewState};
            {error, _Msg} ->
                {{ok, paused}, State#state{status = paused}}
        end,
    {reply, Reply, ReplyState};
handle_call(exploring, _From, State = #state{status = targeting}) ->
    {reply, {ok, targeting}, State};
handle_call({targeting, MsgQueue},
            _From,
            State =
                #state{status = exploring,
                       current_position = CurrentPosition}) ->
    %% TODO take the fist elemnt in queue as it's the only one
    %% (instead of calling find_the_best_target)
    {_Rewards, {BestX, BestY}} = explorer_util:find_the_best_target(CurrentPosition, MsgQueue),
    NewState = State#state{status = targeting},
    targeting(BestX, BestY, State),
    {reply, {ok, NewState}, NewState};
handle_call({targeting, MsgQueue}, _From, State = #state{status = targeting, targeting_mode = dynamic}) ->
    CurrentPosition = State#state.current_position,
    {_Rewards, {BestX, BestY}} = explorer_util:find_the_best_target(CurrentPosition, MsgQueue),
    targeting(BestX, BestY, State),
    {reply, {ok, State}, State};
handle_call({targeting, _MsgQueue}, _From, State = #state{status = targeting, targeting_mode = focus}) ->
    {reply, {ok, State}, State}.

handle_cast({reward_found, {_Rewards, _Coords} = NewTarget}, State) ->
    CurrentTarget = %% get target pos from state
    CurrentMsgQueue = State#state.msg_queue,
    NewMsgQueue = explorer_util:update_msg_queue(NewTarget, CurrentMsgQueue),
    %% TODO Maybe use genserver call to start targeting a new spot
    ServerName = State#state.name,
    targeting(ServerName, NewMsgQueue),
    {noreply, State#state{msg_queue = NewMsgQueue}};

handle_cast({no_rewards, {Rewards, Coords}}, State) ->
    CurrentMsgQueue = State#state.msg_queue,
    NewMsgQueue =
        lists:filter(fun({_, QueueCoords}) -> Coords =/= QueueCoords end, CurrentMsgQueue),
    {noreply, State#state{msg_queue = NewMsgQueue}};

handle_cast(noop, State) ->
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n", [Msg]),
    {noreply, State}.

terminate(Reason, _State) ->
    io:format("Goodbye, brave warriors!~p~n", [Reason]),
    ok.

%%% HELPERS %%%

next_cell_state(NextDirection) ->
    maps:get(NextDirection, gs_war_map:available_steps()).

targeting(X, Y, State) ->
    StateX = State#state.current_position#coord.x,
    StateY = State#state.current_position#coord.y,
    {ok, NewStateH} = targeting_horizontal(X, StateX, State),
    {ok, NewStateV} = targeting_vertical(Y, StateY, NewStateH),

    Coords = NewStateV#state.current_position,
    Neighbours = NewStateV#state.neighbours,
    {Points, _RemRewards} = explorer_util:earn_reward_and_inform(Coords, Neighbours),
    NewState = update_points_in_state(Points, NewStateV),
    NewState.

targeting_horizontal(X, StateX, State) when StateX < X ->
    do_targeting(right, X, State);
targeting_horizontal(X, StateX, State) when StateX > X ->
    do_targeting(left, X, State);
targeting_horizontal(_X, _StateX, State) ->
    {ok, State}.

targeting_vertical(Y, StateY, State) when StateY < Y ->
    do_targeting(down, Y, State);
targeting_vertical(Y, StateY, State) when StateY > Y ->
    do_targeting(up, Y, State);
targeting_vertical(_Y, _StateY, State) ->
    {ok, State}.

do_targeting(Direction, Target, State) ->
    {CellState, Coords} = next_cell_state(Direction),
    case gs_war_map:move(Direction) of
        {ok, moved} ->
            NewState0 = update_state(State, Direction, CellState, Coords),
            {Points, _RemRewards} =
                case CellState of
                    reward -> explorer_util:earn_reward_and_inform(Coords, State#state.neighbours);
                    _ -> {0, 0}
                end,
            NewState = update_points_in_state(Points, NewState0),
            targeting(Direction, Target, NewState);
        {error, Msg} ->
            {{error, Msg}, State}
    end.

update_points_in_state(Points, State) ->
    StatePoints = State#state.points,
    State#state{points = StatePoints + Points}.

exploring(State) ->
    {Direction, CellState, Coords} = compute_next_move(State),
    case gs_war_map:move(Direction) of
        {ok, moved} ->
            {ok, update_state(State, Direction, CellState, Coords), CellState};
        {error, Msg} ->
            {{error, Msg}, State}
    end.

update_state(State, Direction, _CellState, {X, Y}) ->
    PrevMove = State#state.current_position,
    State#state{current_position = #coord{x = X, y = Y},
                current_move = Direction,
                previous_move = PrevMove}.

available_steps() ->
    maps:filter(fun(_Direction, {CellState, _Coords}) -> CellState =/= out end,
                 gs_war_map:available_steps()).

opposite_directions(Dir1, Dir2) ->
    case Dir1 of
        right ->
            Dir2 == left;
        left ->
            Dir2 == right;
        up ->
            Dir2 == down;
        down ->
            Dir2 == up
    end.

compute_next_move(State) ->
    %
    Steps = available_steps(),
    % 1st priority: Go on rewards
    case lists:filter(fun({_Direction, CellState, _Coords}) -> CellState == reward end, Steps)
    of
        [] ->
            % 2nd priority: Go in the same direction
            case lists:filter(fun({Direction, _CellState, _Coords}) ->
                                 Direction == State#state.current_position
                              end,
                              Steps)
            of
                [] ->
                    % 3rd priority: Don’t go backwards
                    case lists:filter(fun(Direction, _CellState, _Coords) ->
                                         opposite_directions(Direction,
                                                             State#state.current_position)
                                         == false
                                      end,
                                      Steps)
                    of
                        [] ->
                            % Go backwards when it’s the only option
                            hd(Steps);
                        RandomSteps ->
                            % Last priority: choose random
                            ListSize = lists:length(RandomSteps),
                            lists:nth(
                                rand:uniform(ListSize), RandomSteps)
                    end;
                [NextMove] ->
                    NextMove
            end;
        Rewards ->
            % 1.1 Go on a reward wich is in the current direction if it exists
            case lists:filter(fun({Direction, _CellState, _Coords}) ->
                                 Direction == State#state.current_position
                              end,
                              Rewards)
            of
                [] ->
                    % 1.2 Don’t go backwards for reward
                    case lists:filter(fun(Direction, _CellState, _Coords) ->
                                         opposite_directions(Direction,
                                                             State#state.current_position)
                                         == false
                                      end,
                                      Rewards)
                    of
                        [] ->
                            % Go backwards when it’s the only option
                            hd(Rewards);
                        RandomRewards ->
                            % Last priority: choose random
                            ListSize = lists:length(RandomRewards),
                            lists:nth(
                                rand:uniform(ListSize), RandomRewards)
                    end;
                [Reward] ->
                    Reward
            end
    end.
