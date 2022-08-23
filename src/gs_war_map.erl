-module(gs_war_map).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([get_dimensions/0, get_coord/0, add_pid/0, add_pid/2]).
-export([move_right/0, move_left/0, move_up/0, move_down/0]).
-export([available_steps/0]).

% -type coord() :: {integer(), integer()}.
-record(coord, {x :: integer(), y :: integer()}).
%-record(process, {pid, name, coords :: coord}).
-record(war_map, {dim_n = 10, dim_m = 10, process_map :: #{term() => coord}, reward_list = [#coord{x=9, y=9}]:: [coord]}).

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_dimensions()->
   gen_server:call(?MODULE, get_dimensions).

get_coord()->
   gen_server:call(?MODULE, get_coord).

add_pid()->
   gen_server:call(?MODULE, {add_pid, 0, 0}).
add_pid(X, Y)->
   gen_server:call(?MODULE, {add_pid, X, Y}).

move_right()->
   gen_server:call(?MODULE, {move, right}).
move_left()->
   gen_server:call(?MODULE, {move, left}).
move_up()->
   gen_server:call(?MODULE, {move, up}).
move_down()->
   gen_server:call(?MODULE, {move, down}).

available_steps()->
   gen_server:call(?MODULE, available_steps).

stop() ->
   gen_server:stop(?MODULE).

% Callbacks
init(_Args) ->
   {ok, #war_map{process_map=#{}}}.

% war_map - Getters
% TODO

% Get dimensions
handle_call(get_dimensions, _From, State) ->
   Reply = {ok, {State#war_map.dim_n, State#war_map.dim_m}},
   {reply, Reply, State};

% Get coord
handle_call(get_coord, From, State) ->
   Reply = case maps:get(From, State#war_map.process_map) of
            Coord -> {ok, {Coord#coord.x, Coord#coord.y}};
            _ -> {badkey, "Out of game"}
   end,
   {reply, Reply, State};

% Add PID
handle_call({add_pid, X, Y}, From, State) ->
   {Pid, _} = From,
   NewState = State#war_map{process_map = #{Pid => #coord{x = X, y = Y}}},
   Reply = {ok, NewState},
   {reply, Reply, NewState};

% Move PID
handle_call({move, Direction}, From, State) ->
   {NewState, Reply} = case move_pid(Direction, From, State) of
      {ok, NewCoord} ->
         {Pid, _} = From,
         ProcessMap = State#war_map.process_map,
         NewProcessMap = ProcessMap#{Pid => NewCoord},
         {State#war_map{process_map = NewProcessMap}, moved};
      {error, ErrorMsg} ->
         {State, {error, ErrorMsg}}
   end,
   {reply, Reply, NewState};

% Available steps
handle_call(available_steps, From, State) ->
   Reply =  {ok, steps_state(From, State)},
   {reply, Reply, State}.

handle_info(Msg, State) ->
   io:format("Unexpected message: ~p~n",[Msg]),
   {noreply, State}.

terminate(Reason, _State) ->
   io:format("Goodbye, brave warriors!~p~n",[Reason]),
   ok.

% Module helper functions

% XoY Axis
% 0 -- > X
% |
% v
% Y

% ? multiple definitions or case of
move_pid(Direction, From, State) ->
   {Pid, _} = From,
   ProcessMap = State#war_map.process_map,
   OldCoord = maps:get(Pid, ProcessMap),
   % ? NewCoord = OldCoord#coord{x = OldCoord#coord.x + 1},
   case Direction of
      right ->
         {ok, #coord{x = OldCoord#coord.x + 1, y = OldCoord#coord.y}};
      left -> 
         {ok, #coord{x = OldCoord#coord.x - 1, y = OldCoord#coord.y}};
      down -> 
         {ok, #coord{x = OldCoord#coord.x, y = OldCoord#coord.y + 1}};
      up -> 
         {ok, #coord{x = OldCoord#coord.x, y = OldCoord#coord.y - 1}};
      _ ->
         {error, "Invalid direction!"}
   end.

is_out(X, Y, State) ->
   (X < 0) orelse (X >= State#war_map.dim_n) orelse (Y < 0) orelse (Y >= State#war_map.dim_m).

is_reward(X, Y, State) ->
   _GivenCoord = #coord{x = X, y = Y},
   case lists:search(fun(Reward) -> Reward == #coord{x = X, y = Y} end, State#war_map.reward_list) of
      {value, _} -> true;
      false -> false
   end.

steps_state(From, State) ->
   % Right, Left, Down, Up
   Directions = [{1, 0}, {-1, 0}, {0, 1}, {0, -1}],
   {Pid, _} = From,
   ProcessMap = State#war_map.process_map,
   Coord = maps:get(Pid, ProcessMap),
   lists:map(
      fun({X, Y}) ->
         NewX = Coord#coord.x + X,
         NewY = Coord#coord.y + Y,
         CoordState = case is_out(NewX, NewY, State) of
            true -> out;
            false ->
               case is_reward(NewX, NewY, State) of
                  true -> reward;
                  false -> empty
               end
         end,
         {{NewX, NewY}, CoordState}
      end
   , Directions). 