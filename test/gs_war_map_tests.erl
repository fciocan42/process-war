-module(gs_war_map_tests).

-include_lib("eunit/include/eunit.hrl").

 
%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%
 
setup_test_() ->
    {
        "Testing the field of war for the brave warriors ...",
        {
            setup,
            fun start/0,
            fun stop/1,
            fun (WarMapPid) ->
                {
                    inparallel,
                    [
                        get_dimensions(WarMapPid)
                    ]
                }
            end
        }
    }.


%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
 
start() ->
    {ok, WarMapPid} = gs_war_map:start_link(),
    WarMapPid.
stop(_WarMapPid) ->
    gs_war_map:stop().


%%%%%%%%%%%%%%%%%%%%
%%% ACTUAL TESTS %%%
%%%%%%%%%%%%%%%%%%%%

get_dimensions(_WarMapPid) ->
    [?_assertEqual({10, 10}, gs_war_map:get_dimensions())].



%%%%%%%%%%%%%%%%%%%%%%%%
%%% HELPER FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%