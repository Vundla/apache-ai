-module(rocket_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, _State) ->
    {cowboy_websocket, Req, #{}}.

websocket_init(State) ->
    %% Subscribe to telemetry via pg
    ok = pg:join(telemetry, self()),
    {ok, State}.

websocket_handle({text, _Msg}, State) ->
    {reply, {text, <<"ok">>}, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({telemetry, Map}, State) ->
    Bin = jsx:encode(Map),
    {reply, {text, Bin}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) -> ok.
-module(rocket_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2]).
-export([websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

init(Req, _State) ->
    {cowboy_websocket, Req, #{}}.

websocket_init(State) ->
    pg:join(rocket_telemetry, self()),
    {ok, State}.

websocket_handle({text, _Msg}, State) ->
    {reply, {text, <<"ok">>}, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({telemetry, Map}, State) when is_map(Map) ->
    %% Very simple JSON encoding without extra deps
    Payload = encode_json(Map),
    {reply, {text, Payload}, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.

encode_json(Map) ->
    Pairs = maps:to_list(Map),
    EncPairs = [<<"\"", K/binary, "\":" , (encode_value(V))/binary>> || {K,V} <- [ {to_bin(K), V} || {K,V} <- Pairs ]],
    Body = join_with(<<",">>, EncPairs),
    <<"{", Body/binary, "}">>.

to_bin(K) when is_atom(K) -> list_to_binary(atom_to_list(K));
to_bin(K) when is_binary(K) -> K;
to_bin(K) when is_list(K) -> list_to_binary(K);
to_bin(K) -> list_to_binary(io_lib:format("~p", [K])).

encode_value(V) when is_integer(V) -> list_to_binary(integer_to_list(V));
encode_value(V) when is_float(V) -> list_to_binary(io_lib:format("~.3f", [V]));
encode_value(V) when is_binary(V) -> <<"\"", V/binary, "\"">>;
encode_value(V) when is_list(V) -> <<"\"", (list_to_binary(V))/binary, "\"">>;
encode_value(V) when is_atom(V) -> <<"\"", (list_to_binary(atom_to_list(V)))/binary, "\"">>;
encode_value(V) when is_map(V) -> encode_json(V);
encode_value(V) -> <<"\"", (list_to_binary(io_lib:format("~p", [V])))/binary, "\"">>.

join_with(_Sep, []) -> <<>>;
join_with(_Sep, [X]) -> X;
join_with(Sep, [X|Xs]) -> <<X/binary, Sep/binary, (join_with(Sep, Xs))/binary>>.
