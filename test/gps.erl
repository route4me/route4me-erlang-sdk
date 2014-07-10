%%%----------------------------------------------------------------------
%%% Rout4me API Erlang Client Library
%%% Example Suite
%%%----------------------------------------------------------------------

-module(gps).
-include("../src/er4cli.hrl").
-compile(export_all). % to use in example suite only

%%%----------------------------------------------------------------------

test_set_gps() ->
	Params = [
        {route_id, "AC16E7D338B551013FF34266FE81A5EE"},
		{format, 'csv'},
        {lat, 33.14384},
        {lng, -83.22466},
        {course, 1},
        {speed, 120},
		{device_type, 'iphone'},
        {member_id, 1},
        {device_guid, "TEST_GPS"},
        {device_timestamp, "2014-06-14 17:43:35"}
	],
	utils:test_print0(<<"Set GPS:">>, er4cli:set_gps(Params)).

test_track_device_last_location_history() ->
	RouteID = "AC16E7D338B551013FF34266FE81A5EE",
	P0 = [
        {route_id, RouteID},
		{format, 'csv'},
        {lng, -83.22466},
        {course, 1},
		{device_type, 'iphone'},
        {member_id, 1},
        {device_guid, "TEST_GPS"}
	],
	Var = [{"2014-06-14 17:40:00", 125, 33.15384},
		   {"2014-06-14 17:45:00", 130, 33.16384}, 
		   {"2014-06-14 17:50:00", 135, 33.17384},
		   {"2014-06-14 17:55:00", 140, 33.18384}],
	[er4cli:set_gps([{device_timestamp, T}, {speed, S}, {lat, L} | P0]) || {T, S, L} <- Var],
	utils:test_print0(<<"Gets Route manifest for GPS location history:">>, er4cli:get_route([{route_id, RouteID},{device_tracking_history, 1}])).

%%-----------------------------------------------------------------------
