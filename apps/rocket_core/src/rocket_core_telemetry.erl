-module(rocket_core_telemetry).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2, handle_call/3, handle_cast/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    %% Create a pg group for telemetry
    ok = pg:join(telemetry, self()),
    %% Tick every 1000ms
    erlang:send_after(1000, self(), tick),
    {ok, #{}}.

handle_info(tick, State) ->
    Ts = erlang:system_time(millisecond),
    %% Generate lightweight synthetic telemetry
    Thrust = 500 + rand:uniform()*10,
    Exhaust = 3000 + rand:uniform()*50,
    Temp = 3200 + rand:uniform()*80,
    Msg = #{ts => Ts, thrust_kN => Thrust, exhaust_velocity_ms => Exhaust, temperature_K => Temp},
    %% Broadcast via pg
    Members = pg:get_members(telemetry),
    lists:foreach(fun(P) -> P ! {telemetry, Msg} end, Members),
    erlang:send_after(1000, self(), tick),
    {noreply, State};
handle_info(_Other, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
-module(rocket_core_telemetry).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    rand:seed(exsplus, erlang:unique_integer()),
    ensure_pg_group(),
    erlang:send_after(1000, self(), tick),
    {ok, #{mass => 1000.0}}.

handle_call(_Req, _From, State) -> {reply, ok, State}.
handle_cast(_Msg, State) -> {noreply, State}.

handle_info(tick, State=#{mass := Mass0}) ->
    Thrust = 50_000.0 + (rand:uniform() * 10_000.0),
    ExhaustV = 2_800.0 + (rand:uniform() * 400.0),
    Temp = 3000.0 + (rand:uniform() * 400.0),
    Mass = max(Mass0 - 0.5 - rand:uniform(), 100.0),
    Ts = erlang:system_time(millisecond),
    Map = #{
        <<"thrust">> => Thrust,
        <<"exhaust_velocity">> => ExhaustV,
        <<"temperature">> => Temp,
        <<"mass">> => Mass,
        <<"ts">> => Ts
    },
    broadcast({telemetry, Map}),
    erlang:send_after(1000, self(), tick),
    {noreply, State#{mass => Mass}};
handle_info(_Other, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

ensure_pg_group() ->
    case lists:member(rocket_telemetry, pg:which_groups()) of
        true -> ok;
        false -> pg:join(rocket_telemetry, self()), ok
    end.

broadcast(Msg) ->
    Pids = pg:get_members(rocket_telemetry),
    lists:foreach(fun(P) -> P ! Msg end, Pids).

max(A, B) when A >= B -> A;
max(_A, B) -> B.
