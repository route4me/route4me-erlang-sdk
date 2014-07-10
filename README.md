# Rout4me API Erlang client

## Installation

The project is a code base, not an application, so it assumes that you will place source files from the code base of the library to your Erlang project and use further according to examples found in er4cli_tests.erl. Rout4me API Erlang client has no external dependencies, but it includes the 3rd party library to deal with JSON (json2.erl).

Rout4me API Erlang client comprises 3 core files:

* er4cli.erl - api calls/wrappers
* er4cli.hrl - defines
* json2.erl  - 3rd party json library

and an additional source file containing exhaustive samples for each section of Rout4me API:

* er4cli_tests.erl

### HTTP Client

The project uses the httpc module coming with Erlang/OTP distribution as a HTTP library, so the HTTP Client API is available when the `inets` application is started. This means that after inserting source codes into your project you should add to your Erlang/OTP application dependency from `inets` application. The application resource file should list 'inets' and 'ssl' within applications which must be started before your application is allowed to be started, like in:

```erlang
  {application, YOUR_APP,
   [{description, "..."},
    ...
    {applications, [kernel, stdlib, sasl, inets, ssl]},
    ...
   ]}.
```

## Creating a Simple Route

```erlang
Addresses = [
    #er4addr{
        address="754 5th Ave New York, NY 10019",
        lat=40.7636197,
        lng=-73.9744388,
        alias="Bergdorf Goodman",
        is_depot=1,
        time=0
    },
    #er4addr{
        address="717 5th Ave New York, NY 10022",
        lat=40.7669692,
        lng=-73.9693864,
        alias="Giorgio Armani",
        time=0
    },
    #er4addr{
        address="888 Madison Ave New York, NY 10014",
        lat=40.7715154,
        lng=-73.9669241,
        alias="Ralph Lauren Women's and Home",
        time=0
    },
    #er4addr{
        address="1011 Madison Ave New York, NY 10075",
        lat=40.7772129,
        lng=-73.9669,
        alias="Yigal Azrou\u00ebl",
        time=0
    },
    #er4addr{
        address="440 Columbus Ave New York, NY 10024",
        lat=40.7808364,
        lng=-73.9732729,
        alias="Frank Stella Clothier",
        time=0
    },
    #er4addr{ 
        address="324 Columbus Ave #1 New York, NY 10023",
        lat=40.7803123,
        lng=-73.9793079,
        alias="Liana",
        time=0
    },
    #er4addr{
        address="110 W End Ave New York, NY 10023",
        lat=40.7753077,
        lng=-73.9861529,
        alias="Toga Bike Shop",
        time=0
    },
    #er4addr{
        address="555 W 57th St New York, NY 10019",
        lat=40.7718005,
        lng=-73.9897716,
        alias="BMW of Manhattan",
        time=0
    },
    #er4addr{
        address="57 W 57th St New York, NY 10019",
        lat=40.7558695,
        lng=-73.9862019,
        alias="Verizon Wireless",
        time=0
    }],
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
    {vehicle_max_distance_mi, 10000}
],
case er4cli:run_optimization(Addresses, Params) of
    {ok, Resp} ->
        io:format("Json object:~n~p~n", [Resp]);
    {error, Why, Details} ->
        io:format("Error: ~p: ~p~n", [Why, Details])
end,
```

### More examples

Please see er4cli_tests.erl for more examples. Functions 'test_addrs_...' provide sample address lists for 'test_...' function, which demonstrate examples of Route & GPS calls and some optimization problems (single driver, round trip, multiple driver, time window, re-optimization).
