%%%----------------------------------------------------------------------
%%% Rout4me API Erlang Client Library
%%% Example Suite
%%%----------------------------------------------------------------------

-module(multiple_driver).
-include("../src/er4cli.hrl").
-compile(export_all). % to use in example suite only

%%%----------------------------------------------------------------------

test_multiple_depot_multiple_driver_time_window() ->
	Params = [
        {route_name, "Multiple Depot, Multiple Driver, Time window"},
        {algorithm_type, 'TSP_TW'},
        {share_route, 0},
        {store_route, 1},
        {device_type, web},
        {distance_unit, mi},
        {travel_mode, 'Driving'},
        {metric, 'ROUTE4ME_METRIC_GEODESIC'},
        {vehicle_capacity, 99},
        {vehicle_max_distance_mi, 99999},
        {parts, 10},
        {route_time, 0},
        {rt, 1},
        {route_max_duration, 86400},
        {optimize, 'Time'}
	],
	utils:test_print0(<<"Multiple Depot, Multiple Driver, Time window:">>, er4cli:run_optimization(utils:test_addrs_TSP_TW(), Params)).

test_multiple_depot_multiple_driver_with_24_stops_time_window() ->
	Params = [
        {route_name, "Multiple Depot, Multiple Driver"},
        {algorithm_type, 'CVRP_TW_MD'},
        {share_route, 0},
        {store_route, 0},
        {route_time, 0},
        {route_max_duration, 86400},
		{vehicle_capacity, 1},
		{vehicle_max_distance_mi, 10000},
        {distance_unit, mi},
        {device_type, web},
        {travel_mode, 'Driving'},
        {metric, 'ROUTE4ME_METRIC_GEODESIC'},
        {optimize, 'Distance'}
	],
	utils:test_print0(<<"Multiple Depot, Multiple Driver (with 24 stops time window):">>, er4cli:run_optimization(utils:test_addrs_TSP_TW(), Params)).

%%-----------------------------------------------------------------------
