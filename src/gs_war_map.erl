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

% fciocan branch
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
   gen_server:call(?MODULE, move_right).
move_left()->
   gen_server:call(?MODULE, move_left).
move_up()->
   gen_server:call(?MODULE, move_up).
move_down()->
   gen_server:call(?MODULE, move_down).

stop() ->
   gen_server:stop(?MODULE).

% Callbacks
init(_Args) ->
   {ok, _WarMap = #war_map{process_map=#{}}}.

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
   NewState = State#war_map{process_map = #{From => #coord{x = X, y = Y}}},
   Reply = {ok, NewState},
   {reply, Reply, NewState};

% XoY Axis
% 0 -- > X
% |
% v
% Y

% Move Right
handle_call(move_right, From, State) ->
   NewState = State#war_map{process_map = #{From => #coord{x = #coord.x + 1}}},
   Reply = {ok, NewState},
   {reply, Reply, NewState};
% Move Left
handle_call(move_left, From, State) ->
   NewState = State#war_map{process_map = #{From => #coord{x = #coord.x - 1}}},
   Reply = {ok, NewState},
   {reply, Reply, NewState};
% Move Up
handle_call(move_up, From, State) ->
   NewState = State#war_map{process_map = #{From => #coord{y = #coord.y - 1}}},
   Reply = {ok, NewState},
   {reply, Reply, NewState};
% Move Down
handle_call(move_down, From, State) ->
   NewState = State#war_map{process_map = #{From => #coord{y = #coord.y + 1}}},
   Reply = {ok, NewState},
   {reply, Reply, NewState}.

handle_info(Msg, State) ->
   io:format("Unexpected message: ~p~n",[Msg]),
   {noreply, State}.

terminate(Reason, _State) ->
   io:format("Goodbye, brave warriors!~p~n",[Reason]),
   ok.