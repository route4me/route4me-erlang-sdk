%%%----------------------------------------------------------------------
%%% Rout4me API Erlang Client Library
%%% 
%%%----------------------------------------------------------------------

% Target API Urls

-define(ER4_TOKEN, "11111111111111111111111111111111").					% insert your default developer token here

-define(ER4_TIMEOUT, 30000).											% 30 secs

-record(er4args,
	{token = ?ER4_TOKEN		:: string(),										% developer token
	 %fields = []			:: [string()],										% fields to request
	 version = 4			:: pos_integer(),									% api version
	 format = json			:: 'undefined' | 'json',							% response format used by Frontpage API
	 method = undefined		:: 'undefined' | 'delete' | 'get' | 'post' | 'put',	% method to use: GET or POST
	 content = <<>>			:: string() | binary(),								% content to send when using POST method
	 timeout = 'infinity'	:: non_neg_integer() | 'infinity'					% httpc timeout
	}).

-record(er4addr,
	{address = ""				:: string(),
	lat = 0						:: float() | string(),
	lng = 0						:: float() | string(),
	alias=undefined				:: 'undefined' | string(),
	is_depot					:: 'undefined' | 0 | 1,
	time = 0					:: integer(),
	time_window_start=undefined	:: 'undefined' | integer(),
	time_window_end=undefined	:: 'undefined' | integer()
	}).

-type(opt_state() :: 1 | 2 | 3 | 4 | 5 | 6).
-type(get_route_q_args() :: {'state', opt_state()} | {'limit', integer()} | {'offset', integer()} | 
							{'device_tracking_history', integer()} | {'route_path_output', atom()} | 
							{'route_id', string()} | {'directions', 0 | 1}).
-type(set_gps() :: {'format', atom()} | {'member_id', integer()} | {'route_id', string()} | 
				   {'course', integer()} | {'speed', float()} | {'lat', float()} | {'lng', float()} | 
				   {'device_type', atom()} | {'device_guid', string()} | {'device_timestamp', string()}).
-type(reoptimize_args() :: {'reoptimize', 0 | 1} | {'optimization_problem_id', string()}).
-type(run_opt_args() ::
	{'algorithm_type',  any()} |
	{'parts',  any()} |
	{'remote_ip',  any()} |
	{'member_id',  any()} |
	{'is_upload',  any()} |
	{'rt',  any()} |
	{'route_name',  any()} |
	{'route_date',  any()} |
	{'route_time',  any()} |
	{'route_max_duration',  any()} |
	{'shared_publicly',  any()} |
	{'disable_optimization',  any()} |
	{'optimize',  any()} |
	{'lock_last',  any()} |
	{'distance_unit',  any()} |
	{'travel_mode',  any()} |
	{'avoid',  any()} |
	{'format',  any()} |
	{'debug_log',  any()} |
	{'store_route',  any()} |
	{'vehicle_id',  any()} |
	{'driver_id',  any()} |
	{'device_id',  any()} |
	{'device_type',  any()} |
	{'dev_lat',  any()} |
	{'dev_lng',  any()} |
	{'metric',  any()} |
	{'route_path_output',  any()} |
	{'vehicle_capacity',  any()} |
	{'vehicle_max_distance_mi',  any()} |
	{'directions',  any()} |
	{'optimized_callback_url',  any()} |
	{'share_email',  any()}
	 ).

-type(json() :: {struct, [{string() | binary(), term()}]} | {array, list()} | string() | number()).
-type(er4_resp() :: {ok, json()} | {ok, raw, string() | binary()} | {error, 'http_code' | 'syntax' | 'general', term()}).
-type(api_set() :: 'opt' | 'gps' | 'route').
-type(field_set() :: {'api_key', string()} |
					 {'optimized_callback_url', string()} |
					 set_gps() |
					 reoptimize_args() |
					 run_opt_args() |
					 get_route_q_args()
	 ).

-define(ER4_DRIVER_VERSION, "route4me-python-driver-0.0.1").
-define(ER4_API_HOSTS, "https://www.route4me.com/api.v4/optimization_problem.php").
-define(ER4_API_HOST, "http://www.route4me.com/api.v4/optimization_problem.php").
-define(ER4_SHOW_ROUTE_HOST, "https://www.route4me.com/route4me.php").
-define(ER4_ROUTE_HOST, "https://www.route4me.com/api.v4/route.php").
-define(ER4_SET_GPS_HOST, "https://www.route4me.com/track/set.php").

-define(OPTIMIZATION_STATE_INITIAL,1).
-define(OPTIMIZATION_STATE_MATRIX_PROCESSING,2).
-define(OPTIMIZATION_STATE_OPTIMIZING,3).
-define(OPTIMIZATION_STATE_OPTIMIZED,4).
-define(OPTIMIZATION_STATE_ERROR,5).
-define(OPTIMIZATION_STATE_COMPUTING_DIRECTIONS,6).

%%%----------------------------------------------------------------------
