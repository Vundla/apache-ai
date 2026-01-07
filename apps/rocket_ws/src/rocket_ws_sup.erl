-module(rocket_ws_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(DEFAULT_PORT, 8080).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ok = ensure_pg_group(),
    Port = application:get_env(rocket_ws, port, ?DEFAULT_PORT),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", rocket_ws_home_h, []},
            {"/ws", rocket_ws_handler, []},
            {"/api/actions", rocket_ws_api_handler, #{mode => actions}},
            {"/api/actions/:id", rocket_ws_api_handler, #{mode => action}},
            {"/api/reactions", rocket_ws_api_handler, #{mode => reactions}},
            {"/api/reactions/:id", rocket_ws_api_handler, #{mode => reaction}}
        ]}
    ]),

    {ok, _} = cowboy:start_clear(rocket_http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}),

    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    {ok, {SupFlags, []}}.

ensure_pg_group() ->
    case lists:member(rocket_telemetry, pg:which_groups()) of
        true -> ok;
        false -> pg:join(rocket_telemetry, self()), ok
    end.
