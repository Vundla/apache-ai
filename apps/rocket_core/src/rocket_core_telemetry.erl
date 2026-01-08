-module(rocket_core_telemetry).
-behaviour(gen_server).
-define(TELEMETRY_GROUP, rocket_telemetry).

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
    Mass = erlang:max(Mass0 - 0.5 - rand:uniform(), 100.0),
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

terminate(_Reason, _State) ->
    pg:leave(?TELEMETRY_GROUP, self()),
    ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.

ensure_pg_group() ->
    pg:join(?TELEMETRY_GROUP, self()).

broadcast(Msg) ->
    Pids = pg:get_members(?TELEMETRY_GROUP),
    lists:foreach(fun(P) -> P ! Msg end, Pids).
