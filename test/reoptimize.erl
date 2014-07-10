%%%----------------------------------------------------------------------
%%% Rout4me API Erlang Client Library
%%% Example Suite
%%%----------------------------------------------------------------------

-module(reoptimize).
-include("../src/er4cli.hrl").
-compile(export_all). % to use in example suite only

%%%----------------------------------------------------------------------

test_reoptimize_byid() ->
	OPId = "c46648541ca5d716a31ffae6f405a37d",
	case er4cli:reoptimize(OPId) of
		{ok, {struct, Props}} ->
			State = proplists:get_value("state", Props),
			io:format("~p: reoptimized Ok. State is ~p (~p)~n", [OPId, er4cli:op_state(State), State]);
		{ok, Resp} ->
			io:format("Reoptimize: Ok. Json object is:~n~p~n", [Resp]);
		{error, Why, Details} ->
			io:format("error (in reoptimize): ~p ~p: ~p: ~p~n", [?MODULE, ?LINE, Why, Details])
	end.

test_optimize_and_reoptimize() ->
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
	case er4cli:run_optimization(utils:test_addrs_single_driver_round_trip(), Params) of
		{ok, {struct, Props}} ->
			OPId = proplists:get_value("optimization_problem_id", Props, "undefined"),
			io:format("Parsed optimization problem ID: ~p~n", [OPId]),
			utils:test_print0(<<"Reoptimize:">>, er4cli:reoptimize(OPId));
		{ok, Resp} ->
			io:format("Reoptimize: Ok. Json object is:~n~p~n", [Resp]);
		{error, Why, Details} ->
			io:format("error (in reoptimize): ~p ~p: ~p: ~p~n", [?MODULE, ?LINE, Why, Details])
	end.

%%-----------------------------------------------------------------------
