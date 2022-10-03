-module(explorer_util).

-export([earn_reward_and_inform/2,
         earn_multiple_rewards_and_inform/3,
         find_the_best_target/2,
         inform_explorers/4]).

-include("records.hrl").

-spec earn_reward_and_inform(coord, list(atom)) -> {integer(), integer()}.
earn_reward_and_inform(Coords, Neighbours) ->
    Mode = application:get_env(process_war, collection_mode, single_reward),
    {Points, RemRewards} =
        case Mode of
            single_reward ->
                earn_single_reward_and_inform(Coords, Neighbours);
            _ ->
                earn_multiple_rewards_and_inform(Coords, 0, Neighbours)
        end,
    {Points, RemRewards}.

earn_single_reward_and_inform(Coords, Neighbours) ->
    {ok, {Point, RemRewards}} = gen_server:call(gs_war_map, {earn_reward, Coords}),
    case {Point, RemRewards} of
        {0, _} ->
            inform_explorers(?NO_REWARDS, Coords, RemRewards, Neighbours);
        {_, 0} ->
            inform_explorers(?NO_REWARDS, Coords, RemRewards, Neighbours);
        _ ->
            inform_explorers(?REWARD_FOUND, Coords, RemRewards, Neighbours)
    end,
    {Point, RemRewards}.

earn_multiple_rewards_and_inform(Coords, Acc, Neighbours) ->
    MapResp = gen_server:call(gs_war_map, {earn_reward, Coords}),
    case MapResp of
        {ok, {0, _RemRewards}} ->
            inform_explorers(?NO_REWARDS, Coords, 0, Neighbours),
            {Acc, _RemRewards};
        {ok, {Point, RemRewards}} ->
            inform_explorers(?REWARD_FOUND, Coords, RemRewards, Neighbours),
            earn_multiple_rewards_and_inform(Coords, Acc + Point, Neighbours)
    end.

inform_explorers(Msg, Coords, ?UNKOWN_REWARDS_NO, Neighbours) ->
    do_inform(Msg, Coords, 1, Neighbours);
inform_explorers(Msg, Coords, RewardsNo, Neighbours) ->
    do_inform(Msg, Coords, RewardsNo, Neighbours).

do_inform(Msg, Coords, RewardsNo, Neighbours) ->
    lists:foreach(fun(Neighbour) ->
        gen_server:cast(Neighbour, {Msg, {RewardsNo, Coords}})
     end,
     Neighbours).

find_the_best_target(CurrentPosition, []) ->
    
find_the_best_target(CurrentPosition, MsgQueue) ->
        lists:sort(fun({R1, Coords1}, {R2, Coords2}) ->
                      hvalue(R1, CurrentPosition, Coords1) > hvalue(R2, CurrentPosition, Coords2)
                   end,
                   MsgQueue).

hvalue(Rewards, CurrentPosition, TargetPostion) ->
        Rewards / compute_distance(CurrentPosition, TargetPostion).

compute_distance({CurrentX, CurrentY}, {TargetX, TargetY}) ->
        math:sqrt(math:pow(CurrentX - TargetX, 2) + math:pow(CurrentY - TargetY, 2)).