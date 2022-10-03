-record(coord, {x :: integer(), y :: integer()}).
-record(war_map,
        {dim_n = 10,
         dim_m = 10,
         process_map :: #{term() => coord},
         reward_map = [#coord{x = 9, y = 9}] :: [coord]}).

-define(REWARD_FOUND, reward_found).
-define(NO_REWARDS, no_rewards).
-define(UNKOWN_REWARDS_NO, unknown_rem_rewards).