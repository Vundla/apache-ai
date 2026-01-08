-module(rocket_ws_handler).
-behaviour(cowboy_websocket).
-define(TELEMETRY_GROUP, rocket_telemetry).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, _State) ->
    {cowboy_websocket, Req, #{}}.

websocket_init(State) ->
    pg:join(?TELEMETRY_GROUP, self()),
    {ok, State}.

websocket_handle({text, _Msg}, State) ->
    {reply, {text, <<"ok">>}, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({telemetry, Map}, State) when is_map(Map) ->
    Payload = jsx:encode(Map),
    {reply, {text, Payload}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    pg:leave(?TELEMETRY_GROUP, self()),
    ok.
