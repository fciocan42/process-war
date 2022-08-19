-module(gs_war_map).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([get_dimensions/0, get_coord/0, add_pid/0, add_pid/2]).
-export([move_right/0, move_left/0, move_up/0, move_down/0]).

% -type coord() :: {integer(), integer()}.
-record(coord, {x :: integer(), y :: integer()}).
%-record(process, {pid, name, coords :: coord}).
-record(war_map, {dim_n = 10, dim_m = 10, process_map :: #{term() => coord}, reward_list = [#coord{x=10, y=10}]:: [coord]}).

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

% Move Right
handle_call({move, Direction}, From, State) ->
   NewState = move_pid(Direction, From, State),
   Reply = {ok, NewState},
   {reply, Reply, NewState}.

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
   % {ok, NewCoord} = case Direction of
   NewCoord = case Direction of
      right ->
         #coord{x = OldCoord#coord.x + 1, y = OldCoord#coord.y};
      left -> 
         #coord{x = OldCoord#coord.x - 1, y = OldCoord#coord.y};
      down -> 
         #coord{x = OldCoord#coord.x, y = OldCoord#coord.y + 1};
      up -> 
         #coord{x = OldCoord#coord.x, y = OldCoord#coord.y - 1};
      _ ->
         {error, "Invalid direction!"}
   end,
   NewProcessMap = ProcessMap#{Pid => NewCoord},
   State#war_map{process_map = NewProcessMap}.
   % ?before NewState = State#war_map{process_map = #{Pid => NewCoord}},