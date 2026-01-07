-module(rocket_ws_api_handler).

-export([init/2]).

-define(JSON_HEADERS, #{<<"content-type">> => <<"application/json">>}).

init(Req0, State) ->
    Mode = maps:get(mode, State),
    Method = cowboy_req:method(Req0),
    {Status, Body} = route(Method, Mode, Req0),
    Req1 = cowboy_req:reply(Status, ?JSON_HEADERS, Body, Req0),
    {ok, Req1, State}.

route(<<"GET">>, actions, _Req) ->
    {200, encode(#{<<"actions">> => actions_catalog()})};
route(<<"GET">>, reactions, _Req) ->
    {200, encode(#{<<"reactions">> => reactions_catalog()})};
route(<<"GET">>, action, Req) ->
    respond_entry(fun find_action/1, fun action_payload/1, Req);
route(<<"GET">>, reaction, Req) ->
    respond_entry(fun find_reaction/1, fun reaction_payload/1, Req);
route(_, _, _Req) ->
    {405, encode(#{<<"error">> => <<"method_not_allowed">>})}.

respond_entry(Finder, Wrapper, Req) ->
    case binding_id(Req) of
        {ok, Id} ->
            case Finder(Id) of
                undefined -> {404, encode(#{<<"error">> => <<"unknown_action">>, <<"id">> => Id})};
                Entry -> {200, encode(Wrapper(Entry))}
            end;
        error -> {404, encode(#{<<"error">> => <<"unknown_action">>})}
    end.

action_payload(Entry) -> #{<<"action">> => Entry}.
reaction_payload(Entry) -> #{<<"reaction">> => Entry}.

binding_id(Req) ->
    case cowboy_req:binding(id, Req) of
        undefined -> error;
        Bin ->
            try {ok, binary_to_integer(Bin)} catch _:_ -> error end
    end.

encode(Payload) -> jsx:encode(Payload).

actions_catalog() ->
    [action_entry(Entry) || Entry <- catalog()].

reactions_catalog() ->
    [reaction_entry(Entry) || Entry <- catalog()].

find_action(Id) ->
    case lists:keyfind(Id, 1, catalog()) of
        false -> undefined;
        Entry -> action_entry(Entry)
    end.

find_reaction(Id) ->
    case lists:keyfind(Id, 1, catalog()) of
        false -> undefined;
        Entry -> reaction_entry(Entry)
    end.

reaction_endpoint(Id) -> <<"/api/reactions/", (integer_to_binary(Id))/binary>>.

action_entry({Id, Action, Reaction}) ->
    #{
        <<"id">> => Id,
        <<"action">> => Action,
        <<"reaction_hint">> => Reaction,
        <<"reaction_endpoint">> => reaction_endpoint(Id)
    }.

reaction_entry({Id, Action, Reaction}) ->
    #{
        <<"id">> => Id,
        <<"action_source">> => Action,
        <<"reaction">> => Reaction,
        <<"status">> => <<"paired">>
    }.

catalog() ->
    [
        {1, <<"ignite_main_bus">>, <<"stabilize_core_plasma">>},
        {2, <<"trim_attitude_microjets">>, <<"counter_precession">>},
        {3, <<"pulse_radar_fold">>, <<"calibrate_reflection_grid">>},
        {4, <<"deploy_veil_shield">>, <<"throttle_heat_bloom">>},
        {5, <<"charge_qubit_buffer">>, <<"scrub_entanglement_noise">>},
        {6, <<"prime_holo_nav">>, <<"reconcile_orbitals">>},
        {7, <<"latency_bleed_off">>, <<"lock_relay_harmonics">>},
        {8, <<"seed_hidden_observer">>, <<"hydrate_cache_cluster">>},
        {9, <<"broadcast_pg_wave">>, <<"sample_ws_clients">>},
        {10, <<"sweep_rocket_pg">>, <<"rebalance_node_scopes">>},
        {11, <<"seal_cockroach_ring">>, <<"rotate_sql_creds">>},
        {12, <<"light_cassandra_tls">>, <<"audit_keystore_pair">>},
        {13, <<"refresh_postgres_slot">>, <<"checkpoint_lsn">>},
        {14, <<"dampen_kafka_storm">>, <<"elevate_acl_watch">>},
        {15, <<"spawn_guardian_consumer">>, <<"validate_scram_secret">>},
        {16, <<"fanout_change_feed">>, <<"hash_payload_fingerprint">>},
        {17, <<"jitter_replication_window">>, <<"back_off_selector">>},
        {18, <<"boost_ws_rate">>, <<"buffer_ws_backpressure">>},
        {19, <<"prewarm_frontier_edge">>, <<"pin_next_route_cache">>},
        {20, <<"ignite_next_api">>, <<"mirror_express_contract">>},
        {21, <<"commit_guard_playbook">>, <<"emit_full_audit">>}
    ].