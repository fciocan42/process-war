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
         msg_queue :: [{integer(), coord}],
         neighbours :: [term()],
         targeting_mode :: atom()}).

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

targeting(Name) ->
    gen_server:call(Name, targeting).

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
handle_call(targeting,
            _From,
            State =
                #state{status = exploring,
                       msg_queue = MsgQueue,
                       current_position = CurrentPosition}) ->
    {Rewards, BestX, BestY} = find_the_best_target(CurrentPosition, MsgQueue),
    NewState = State#state{status = targeting},
    {reply, {ok, NewState}, NewState};
handle_call(targeting, _From, State = #state{status = targeting}) ->
    % TODO get  coords from queue
    % targeting(X, Y, State),
    % {reply, {ok, NewState}, NewState}.
    {reply, {ok, State}, State}.

%% MaybeTODO
%% update the other explorers with the number of rewards in the cell after collecting each reward

%% TODO
%% new handle cast for no more rewards
%% eliminate the target from msg queue based on the coordinates
handle_cast({reward_found, {Rewards, Coords} = NewTarget}, State) ->
    CurrentMsgQueue = State#state.msg_queue,
    NewMsgQueue =
        lists:map(fun ({_, QueueCoords}) when QueueCoords == Coords ->
                          NewTarget;
                      (Target) ->
                          Target
                  end,
                  CurrentMsgQueue),
    %% TODO Maybe use genserver call to start targeting a new spot
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

% TODO Map instead of tuple list for direction
next_cell_state(NextDirection) ->
    [NextCellState] =
        lists:filter(fun({Direction, _CellState, _Coords}) -> Direction == NextDirection end,
                     gs_war_map:available_steps()),
    NextCellState.

targeting(X, Y, State) ->
    StateX = State#state.current_position#coord.x,
    StateY = State#state.current_position#coord.y,
    {ok, NewStateH} = targeting_horizontal(X, StateX, State),
    {ok, NewStateV} = targeting_vertical(Y, StateY, NewStateH),
    NewStateV.

% TODO Clause when it arrives on reward coords and try to earn points
targeting_horizontal(X, StateX, State) when StateX < X ->
    {Direction, CellState, Coords} = next_cell_state(right),
    % move right
    case gs_war_map:move(right) of
        {ok, moved} ->
            % TODO check if it s reward to earn and alert
            NewState = update_state(State, Direction, CellState, Coords),
            NewStateX = Coords#coord.x,
            targeting_horizontal(X, NewStateX, NewState);
        {error, Msg} ->
            {{error, Msg}, State}
    end;
targeting_horizontal(X, StateX, State) when StateX > X ->
    {Direction, CellState, Coords} = next_cell_state(left),
    % move left
    case gs_war_map:move(left) of
        {ok, moved} ->
            % TODO check if it s reward to earn and alert
            NewState = update_state(State, Direction, CellState, Coords),
            NewStateX = Coords#coord.x,
            targeting_horizontal(X, NewStateX, NewState);
        {error, Msg} ->
            {{error, Msg}, State}
    end;
targeting_horizontal(X, StateX, State) ->
    {ok, State}.

targeting_vertical(Y, StateY, State) when StateY < Y ->
    {Direction, CellState, Coords} = next_cell_state(down),
    % move down
    case gs_war_map:move(down) of
        {ok, moved} ->
            % TODO check if it s reward to earn and alert
            NewState = update_state(State, Direction, CellState, Coords),
            NewStateY = Coords#coord.y,
            targeting_vertical(Y, NewStateY, NewState);
        {error, Msg} ->
            {{error, Msg}, State}
    end;
targeting_vertical(Y, StateY, State) when StateY > Y ->
    {Direction, CellState, Coords} = next_cell_state(up),
    % move up
    case gs_war_map:move(up) of
        {ok, moved} ->
            % TODO check if it s reward to earn and alert
            NewState = update_state(State, Direction, CellState, Coords),
            NewStateY = Coords#coord.y,
            targeting_vertical(Y, NewStateY, NewState);
        {error, Msg} ->
            {{error, Msg}, State}
    end;
targeting_vertical(Y, StateY, State) ->
    {ok, State}.

exploring(State) ->
    {Direction, CellState, Coords} = compute_next_move(State),
    case gs_war_map:move(Direction) of
        {ok, moved} ->
            {ok, update_state(State, Direction, CellState, Coords), CellState};
        {error, Msg} ->
            {{error, Msg}, State}
    end.

update_state(State, Direction, _CellState, {X, Y}) ->
    % TODO  Pick reward and add point only if the reward is available
    %
    % PointsNow = State#state.points,
    % Points = case CellState of
    %     reward -> PointsNow + 1;
    %     _ -> PointsNow
    % end,
    %
    PrevMove = State#state.current_position,
    State#state{current_position = #coord{x = X, y = Y},
                current_move = Direction,
                previous_move = PrevMove}.

available_steps() ->
    lists:filter(fun({_Direction, CellState, _Coords}) -> CellState =/= out end,
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

find_the_best_target(CurrentPosition, MsgQueue) ->
    lists:sort(fun({R1, Coords1}, {R2, Coords2}) ->
                  hvalue(R1, CurrentPosition, Coords1) > hvalue(R2, CurrentPosition, Coords2)
               end,
               MsgQueue).

hvalue(Rewards, CurrentPosition, TargetPostion) ->
    Rewards / compute_distance(CurrentPosition, TargetPostion).

compute_distance({CurrentX, CurrentY}, {TargetX, TargetY}) ->
    math:sqrt(math:pow(CurrentX - TargetX, 2) + math:pow(CurrentY - TargetY, 2)).
