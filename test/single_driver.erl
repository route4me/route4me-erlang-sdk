%%%----------------------------------------------------------------------
%%% Rout4me API Erlang Client Library
%%% Example Suite
%%%----------------------------------------------------------------------

-module(single_driver).
-include("../src/er4cli.hrl").
-compile(export_all). % to use in example suite only

%%%----------------------------------------------------------------------

test_single_driver_round_trip() ->
	Params = [
        {route_name, "Single Driver Round Trip"},
        {algorithm_type, 'TSP'},
		{remote_ip, 0},
		{member_id, 1},
        {route_time, 0},
        {route_max_duration, 86400},
        {optimize, 'Distance'},
        {distance_unit, mi},
        {travel_mode, 'Driving'},
        {store_route, true},
        {device_type, web},
        {vehicle_capacity, 1},
        {vehicle_max_distance_mi, 10000},
		{directions, 1}
	],
	utils:test_print0(<<"Single Driver Round Trip:">>, er4cli:run_optimization(utils:test_addrs_single_driver_round_trip(), Params)).

test_single_driver_route_10_stops() ->
	Params = [
        {route_name, "Single Driver Route 10 Stops"},
        {algorithm_type, 'TSP'},
		{share_route, 0},
        {optimize, 'Distance'},
        {distance_unit, mi},
        {device_type, web}
	],
	utils:test_print0(<<"Single Driver Route 10 Stops:">>, er4cli:run_optimization(utils:test_addrs_single_driver_route_10_stops(), Params)).

%%-----------------------------------------------------------------------
