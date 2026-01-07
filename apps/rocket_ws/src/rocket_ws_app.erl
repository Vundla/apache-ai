-module(rocket_ws_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    rocket_ws_sup:start_link().

stop(_State) -> ok.
-module(rocket_ws_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rocket_ws_sup:start_link().

stop(_State) ->
    ok.
