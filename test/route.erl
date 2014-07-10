%%%----------------------------------------------------------------------
%%% Rout4me API Erlang Client Library
%%% Example Suite
%%%----------------------------------------------------------------------

-module(route).
-include("../src/er4cli.hrl").
-compile(export_all). % to use in example suite only

%%%----------------------------------------------------------------------

test_get_route_q() ->
	utils:test_print0(<<"Gets a status update on the pending optimization problems:">>, er4cli:get_route_q()).

test_get_route() ->
	utils:test_print0(<<"Gets a Route info by ID:">>, er4cli:get_route_by_id("AC16E7D338B551013FF34266FE81A5EE")).

test_delete_route_non_existing() ->
	utils:test_print0(<<"Deletes a Route by non-existing ID:">>, er4cli:delete_route("AC16E7D338B551013FF34266FE81A5EEXXX")).

test_get_route_manifest() ->
	Params = [
        {route_id, "AC16E7D338B551013FF34266FE81A5EE"},
        {directions, 1},
        {route_path_output, 'Points'},
        {device_tracking_history, 1},
        {limit, 10},
        {offset, 5}
	],
	utils:test_print0(<<"Gets Route manifest:">>, er4cli:get_route(Params)).

%%-----------------------------------------------------------------------
