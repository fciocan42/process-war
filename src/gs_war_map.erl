-module(gs_war_map).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([get_dimensions/0, get_proccess_list/0, get_coord/0, add_pid/0, add_pid/2]).
-export([move/1]).
-export([available_steps/0]).

% -type coord() :: {integer(), integer()}.
-record(coord, {x :: integer(), y :: integer()}).
%-record(process, {pid, name, coords :: coord}).
-record(war_map, {dim_n = 10, dim_m = 10, process_map :: #{term() => coord}, reward_map = #{#coord{x=9, y=9} => 3}:: #{coord => term()}}).

start_link() ->
   gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_dimensions()->
   {ok, Dimensions} = gen_server:call(?MODULE, get_dimensions),
   Dimensions.

get_coord()->
   gen_server:call(?MODULE, get_coord).

get_proccess_list()->
   gen_server:call(?MODULE, get_proccess_list).

add_pid()->
   gen_server:call(?MODULE, {add_pid, 0, 0}).
add_pid(X, Y)->
   gen_server:call(?MODULE, {add_pid, X, Y}).


move(Direction)->
   gen_server:call(?MODULE, {move, Direction}).

available_steps()->
   gen_server:call(?MODULE, available_steps).

stop() ->
   gen_server:stop(?MODULE).

% Callbacks
init(_Args) ->
   {ok, #war_map{process_map=#{}}}.


get_name()->
   gen_server:call(?MODULE, get_name).


% Display map

display_map(State, xc, yc) when xc == coord.x && yc == coord.y && yc < dim_m ->
   io:format("*"),
   display(State, xc, yc+1).
display_map(State, xc, yc) when xc == coord.x && yc == coord.y && yc == dim_m ->
   io:format("*"),
   display(State, xc+1, 0).
display_map(State, xc, yc) when yc < dim_m ->
   io:format("."),
   display(State, xc, yc+1).
display_map(State, xc, yc) when yc == dim_m ->
   io:format("."),
display(State, xc+1, 0).


%%% Getters %%%


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

% Get all processes
handle_call(get_process_list, _From, State) ->
   ProcessList = maps:keys(State#war_map.process_map),
   Reply = {ok, ProcessList},
   {reply, Reply, State};

%Get name
handle_call(get_name, From, State) ->
   Reply = {ok, State#process.name},
   {reply, Reply, State};

% Add PID
handle_call({add_pid, X, Y}, From, State) ->
   %  if I return the state the Process will have all info about map
   {Pid, _} = From,
   NewState = State#war_map{process_map = #{Pid => #coord{x = X, y = Y}}},
   Reply = {ok, NewState},
   {reply, Reply, NewState};

handle_call(neighbours, _From, State) ->
   maps:keys(State#war_map.process_map);

% Move PID
handle_call({move, Direction}, From, State) ->
   {NewState, Reply} = case move_pid(Direction, From, State) of
      {ok, NewCoord} ->
         {Pid, _} = From,
         ProcessMap = State#war_map.process_map,
         NewProcessMap = ProcessMap#{Pid => NewCoord},
         {State#war_map{process_map = NewProcessMap}, {ok, moved}};
      {error, ErrorMsg} ->
         {State, {error, ErrorMsg}}
   end,
   {reply, Reply, NewState};

% Available steps
handle_call(available_steps, From, State) ->
   Reply =  {ok, steps_state(From, State)},
   {reply, Reply, State};

% TODO cast
handle_call({earn_reward, {X, Y}}, _From, State) ->
   {Points, ReplyState} = case is_reward(X, Y, State) of
      true  ->
         % TODO get the amount from reward_map
         RewardAmount = maps:get(#coord{x = X, y = Y}, State#war_map.reward_map, -1),
         NewState = State#war_map{reward_map = #{#coord{x = X, y =  Y} => RewardAmount - 1}},
         {1, NewState};
      false ->
         {0,  State}
   end,
   {reply, {ok, Points}, ReplyState}.

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
   case lists:search(fun(Reward) -> Reward == #coord{x = X, y = Y} end, maps:keys(State#war_map.reward_map)) of
      {value, _} -> true;
      false -> false
   end.

steps_state(From, State) ->
   % Right, Left, Down, Up
   Directions = [{{1, 0},right}, {{-1, 0},left}, {{0, 1},down}, {{0, -1},up}],
   {Pid, _} = From,
   ProcessMap = State#war_map.process_map,
   Coord = maps:get(Pid, ProcessMap),
   lists:map(
      fun({{X, Y}, Direction}) ->
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
         {Direction, CoordState, {NewX, NewY}}
      end
   , Directions).
