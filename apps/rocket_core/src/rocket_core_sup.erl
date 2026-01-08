-module(rocket_core_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    Children = [
        #{id => rocket_core_telemetry,
          start => {rocket_core_telemetry, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => worker,
          modules => [rocket_core_telemetry]}
    ],
    {ok, {SupFlags, Children}}.
