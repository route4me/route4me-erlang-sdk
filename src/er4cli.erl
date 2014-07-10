%%%----------------------------------------------------------------------
%%% Rout4me API Erlang Client Library
%%% 
%%%----------------------------------------------------------------------

-module(er4cli).
-include("er4cli.hrl").

-export([get_route_q/0, get_route_q/1]).
-export([get_route/1, get_route_by_id/1, delete_route/1, set_gps/1]).
-export([run_optimization/2, reoptimize/1]).

-export([op_state/1]).

%%%----------------------------------------------------------------------
%%% Route API
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% @doc
%%  Gets a status update on all the pending (queued) optimization problems for a specific API key.
%%  Params:
%%    State (optional) - use OPTIMIZATION_STATE_XXX defines
%%    Props - list of {name, value} where get_route_q_args() type gives exact namings
%% @end
%%-----------------------------------------------------------------------
-spec get_route_q() -> er4_resp().
get_route_q() ->
	get_route_q([{limit, 10}, {offset, 0}]).

-spec get_route_q(Props :: opt_state() | [get_route_q_args()]) -> er4_resp().
get_route_q(State) when is_integer(State) ->
	call_er4(opt, #er4args{method = get}, [{state, State}]);
get_route_q(Props) when is_list(Props) ->
	call_er4(opt, #er4args{method = get}, Props).

%%-----------------------------------------------------------------------
%% @doc
%%  Gets a Route by ID.
%%  Params:
%%    RouteId - id of the route
%% @end
%%-----------------------------------------------------------------------
-spec get_route_by_id(Id :: string()) -> er4_resp().
get_route_by_id(Id) ->
	call_er4(route, #er4args{method = get}, [{route_id, Id}]).

%%-----------------------------------------------------------------------
%% @doc
%%  Gets info about a Route.
%%  Params:
%%    Props - route properties
%% @end
%%-----------------------------------------------------------------------
-spec get_route(Props :: [get_route_q_args()]) -> er4_resp().
get_route(Props) ->
	call_er4(route, #er4args{method = get}, Props).

%%-----------------------------------------------------------------------
%% @doc
%%  Deletes a Route.
%%  Params:
%%    RouteId - id of the route
%% @end
%%-----------------------------------------------------------------------
-spec delete_route(Id :: string()) -> er4_resp().
delete_route(Id) ->
	call_er4(route, #er4args{method = delete}, [{route_id, Id}]).

%%-----------------------------------------------------------------------
%% @doc
%%  Set GPS point.
%%  Params:
%%    Props - GPS point properties
%% @end
%%-----------------------------------------------------------------------
-spec set_gps(Props :: [set_gps()]) -> er4_resp().
set_gps(Props) ->
	call_er4(gps, #er4args{method = get}, Props).

%%%----------------------------------------------------------------------
%%% Optimization API
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% @doc
%%  Solve optimization problem.
%%  Params:
%%    Addresses - list of addresses
%%    Params - list of optimization parameters
%% @end
%%-----------------------------------------------------------------------
-spec run_optimization(Addresses :: [#er4addr{}], Params :: [run_opt_args()]) -> er4_resp().
run_optimization(Addresses, Params0) ->
	Q = [directions, optimized_callback_url],
	{Aux, Params} = lists:foldl(fun (I, {Acc, PL}) ->
		case proplists:get_value(I, PL) of
			undefined -> {Acc, PL};
			V -> {[{I, V} | Acc], proplists:delete(I, PL)}
		end
	end, {[], Params0}, Q),
	P = [opt_param(X) || X <- Params],
	D = {struct, [{"addresses", {array, [addr2json(X) || X <- Addresses]}}, {"parameters", {struct, P}}]},
	call_er4(opt, #er4args{method = post, content = lists:flatten(json2:encode(D))}, Aux).

%%-----------------------------------------------------------------------
%% @doc
%%  Reoptimize the problem.
%%  Params:
%%    OPId - ID of the optimization problem
%% @end
%%-----------------------------------------------------------------------
-spec reoptimize(OPId :: string()) -> er4_resp().
reoptimize(OPId) ->
	call_er4(opt, #er4args{method = put, content = ""}, [{optimization_problem_id, OPId}, {reoptimize, 1}]).

%%%----------------------------------------------------------------------
%%% Internal api
%%%----------------------------------------------------------------------

%%-----------------------------------------------------------------------
%% @doc
%%  Construct a request.
%%  Params:
%%    ApiUrl - Diffbot API Url
%%    Args   - call Args
%%    Params - list of values, each represents param to pass to Diffbot API
%%             in addition to token and url params; each value is of type
%%             field_set() from er4cli.hrl, and is either atom for most
%%             API calls, or {key, value} pair for Crawlbot/Bulk APIs
%% @end
%%-----------------------------------------------------------------------
-spec make_api_url(ApiUrl :: string(), Args :: #er4args{}, Params :: [field_set()]) -> string().
make_api_url(ApiUrl, Args, Params) ->
	Tail = ["api_key=", http_uri:encode(Args#er4args.token)],
	L = lists:foldl(fun (P, Acc) -> make_arg(P, Args, Acc) end, Tail, Params),
	lists:concat([ApiUrl, '?' | L]).

%%-----------------------------------------------------------------------
%% @doc
%%  Map api ID to url
%% @end
%%-----------------------------------------------------------------------
-spec api_url(api_set()) -> string().
api_url(opt) -> ?ER4_API_HOST;
api_url(gps) -> ?ER4_SET_GPS_HOST;
api_url(route) -> ?ER4_ROUTE_HOST.

%%-----------------------------------------------------------------------
%% @doc
%%  Fulfil a request.
%%  Params:
%%    ApiId  - one of api_set() from er4cli.hrl
%%    Args   - call Args
%%    Params - list of atoms, each represents param to pass to Diffbot API
%%             in addition to token and url params (of field_set() from er4cli.hrl)
%% @end
%%-----------------------------------------------------------------------
-spec call_er4(ApiId :: api_set(), Args :: #er4args{}, Params :: [field_set()]) -> er4_resp().
call_er4(ApiId, Args = #er4args{method = Meth}, Params) ->
	Url = make_api_url(api_url(ApiId), Args, Params),
	Req = case Meth of
		post -> {Url, [], "application/json", Args#er4args.content};
		put  -> {Url, [], "application/json", Args#er4args.content};
		_    -> {Url, []}
	end,
	Opts = [{timeout, Args#er4args.timeout}],
	R = httpc:request(Meth, Req, Opts, []),
	reply(R, Args).

%%-----------------------------------------------------------------------
%% @doc
%%  Convert text reply into json object, or format error reason on fail.
%% @end
%%-----------------------------------------------------------------------
-spec reply({'ok', {{string(), integer(), string()}, [{string(), string()}], string() | binary()} | 
					{integer(), string() | binary()}} | 
			{error, term()}, Args :: #er4args{}) -> er4_resp().

reply({ok, {{_HttpVersion, 200, _}, _RespHeaders, Body}}, A) ->
	reply_parse(Body, A);
reply({ok, {{_HttpVersion, 303, _}, RespHeaders, Body}}, A = #er4args{method = put, timeout = T}) ->
	case proplists:get_value("location", RespHeaders) of
		undefined ->
			reply_parse(Body, A);
		Loc -> 
			R = httpc:request(get, {Loc, []}, [{timeout, T}], []),
			reply(R, A#er4args{method = get})
	end;
reply({ok, {{_HttpVersion, 404, _}, _RespHeaders, Body}}, A) ->
	{error, general, reply_parse(Body, A)};
reply({ok, {200, Body}}, A) ->
	reply_parse(Body, A);
reply({ok, {StatusCode, _Body}}, _A) ->
	{error, http_code, StatusCode};
reply({ok, {{_HttpVersion, StatusCode, _}, _RespHeaders, _Body}}, _A) ->
	{error, http_code, StatusCode};
reply({ok, _RawResp}, _A) ->
	{error, syntax, _RawResp};
reply({error, Reason}, _A) ->
	{error, general, Reason}.

-spec reply_parse(Body :: string() | binary(), Args :: #er4args{}) -> er4_resp().
%reply_parse(Body, #er4args{format = Fmt}) when Fmt =/= json ->
%	{ok, raw, Body}; % raw response, do not try to parse
reply_parse(Body, A) when is_binary(Body) ->
	reply_parse(binary_to_list(Body), A);
reply_parse(Body, _A) ->
	try
		case json2:decode_string(Body) of
			{error, Why} -> {error, json_decode, Why};
			R -> R
		end
	catch
		error:Reason -> {error, json_decode, Reason}
	end.

%%-----------------------------------------------------------------------
%% @doc
%%  Wrap arguments for json/uri utils
%% @end
%%-----------------------------------------------------------------------

-spec make_arg(field_set(), #er4args{}, [term()]) -> [term()].
make_arg({_, []}, _Args, Acc) -> Acc;
make_arg({P = directions, 0}, _Args, Acc) -> [P, '=', 0, '&' | Acc];
make_arg({P = directions, 1}, _Args, Acc) -> [P, '=', 1, '&' | Acc];
make_arg({P = optimized_callback_url, V}, _Args, Acc) -> [P, '=', http_uri:encode(V), '&' | Acc];
make_arg({P = reoptimize, 0}, _Args, Acc) -> [P, '=', 0, '&' | Acc];
make_arg({P = reoptimize, 1}, _Args, Acc) -> [P, '=', 1, '&' | Acc];
make_arg({P = optimization_problem_id, V}, _Args, Acc) -> [P, '=', http_uri:encode(V), '&' | Acc];
make_arg({P = route_id, V}, _Args, Acc) -> [P, '=', http_uri:encode(V), '&' | Acc];
make_arg({P = api_key, V}, _Args, Acc) -> [P, '=', http_uri:encode(V), '&' | Acc];
make_arg({P = device_timestamp, V}, _Args, Acc) -> [P, '=', http_uri:encode(V), '&' | Acc];
make_arg({P = device_guid, V}, _Args, Acc) -> [P, '=', http_uri:encode(V), '&' | Acc];
make_arg({P, V}, _Args, Acc) when is_integer(V) -> [P, '=', V, '&' | Acc];
make_arg({P, V}, _Args, Acc) when is_float(V) -> [P, '=', float_to_list(V, [{decimals, 12}, compact]), '&' | Acc];
make_arg({P, V}, _Args, Acc) when is_atom(V) -> [P, '=', http_uri:encode(atom_to_list(V)), '&' | Acc].

-spec opt_param(run_opt_args()) -> {binary(), string()}.
opt_param({P, V}) when is_atom(V) -> {atom_to_binary(P, utf8), a2s(V)};
opt_param({P, V}) when is_integer(V) -> {atom_to_binary(P, utf8), integer_to_list(V)};
opt_param({P, V}) when is_list(V) -> {atom_to_binary(P, utf8), V}.

-spec a2s(atom()) -> string().
a2s('ROUTE4ME_METRIC_EUCLIDEAN') -> "1";
a2s('ROUTE4ME_METRIC_MANHATTAN') -> "2";
a2s('ROUTE4ME_METRIC_GEODESIC') -> "3";
a2s('ROUTE4ME_METRIC_MATRIX') -> "4";
a2s('ROUTE4ME_METRIC_EXACT_2D') -> "5";

a2s('TSP') -> "1";
a2s('VRP') -> "2";
a2s('CVRP_TW_SD') -> "3";
a2s('CVRP_TW_MD') -> "4";
a2s('TSP_TW') -> "5";
a2s('TSP_TW_CR') -> "6";
a2s('BBCVRP') -> "7";

a2s(V) -> atom_to_list(V).

-spec addr2json(#er4addr{}) -> json().

addr2json(X = #er4addr{}) ->
	{struct,
		[{"address", X#er4addr.address},
		 {"lat", i(X#er4addr.lat)},
		 {"lng", i(X#er4addr.lng)},
		 {"time", i(X#er4addr.time)}]
			++ ad2j("alias", X#er4addr.alias)
			++ ad2j("is_depot", X#er4addr.is_depot)
			++ ad2j("time_window_start", X#er4addr.time_window_start)
			++ ad2j("time_window_end", X#er4addr.time_window_end)}.

ad2j(_, undefined) -> [];
ad2j(N, V) -> [{N, V}].

i(I) when is_float(I) -> float_to_list(I, [{decimals, 12}, compact]);
i(I) when is_integer(I) -> integer_to_list(I);
i(I) when is_list(I) -> I.

%%-----------------------------------------------------------------------
%% @doc
%%  Report utils
%% @end
%%-----------------------------------------------------------------------

op_state(?OPTIMIZATION_STATE_INITIAL) -> 'OPTIMIZATION_STATE_INITIAL';
op_state(?OPTIMIZATION_STATE_MATRIX_PROCESSING) -> 'OPTIMIZATION_STATE_MATRIX_PROCESSING';
op_state(?OPTIMIZATION_STATE_OPTIMIZING) -> 'OPTIMIZATION_STATE_OPTIMIZING';
op_state(?OPTIMIZATION_STATE_OPTIMIZED) -> 'OPTIMIZATION_STATE_OPTIMIZED';
op_state(?OPTIMIZATION_STATE_ERROR) -> 'OPTIMIZATION_STATE_ERROR';
op_state(?OPTIMIZATION_STATE_COMPUTING_DIRECTIONS) -> 'OPTIMIZATION_STATE_COMPUTING_DIRECTIONS';
op_state(_) -> undefined.

%%-----------------------------------------------------------------------
