export const actionReactionPairs = [
    { id: 1, action: 'ignite_main_bus', reaction: 'stabilize_core_plasma' },
    { id: 2, action: 'trim_attitude_microjets', reaction: 'counter_precession' },
    { id: 3, action: 'pulse_radar_fold', reaction: 'calibrate_reflection_grid' },
    { id: 4, action: 'deploy_veil_shield', reaction: 'throttle_heat_bloom' },
    { id: 5, action: 'charge_qubit_buffer', reaction: 'scrub_entanglement_noise' },
    { id: 6, action: 'prime_holo_nav', reaction: 'reconcile_orbitals' },
    { id: 7, action: 'latency_bleed_off', reaction: 'lock_relay_harmonics' },
    { id: 8, action: 'seed_hidden_observer', reaction: 'hydrate_cache_cluster' },
    { id: 9, action: 'broadcast_pg_wave', reaction: 'sample_ws_clients' },
    { id: 10, action: 'sweep_rocket_pg', reaction: 'rebalance_node_scopes' },
    { id: 11, action: 'seal_cockroach_ring', reaction: 'rotate_sql_creds' },
    { id: 12, action: 'light_cassandra_tls', reaction: 'audit_keystore_pair' },
    { id: 13, action: 'refresh_postgres_slot', reaction: 'checkpoint_lsn' },
    { id: 14, action: 'dampen_kafka_storm', reaction: 'elevate_acl_watch' },
    { id: 15, action: 'spawn_guardian_consumer', reaction: 'validate_scram_secret' },
    { id: 16, action: 'fanout_change_feed', reaction: 'hash_payload_fingerprint' },
    { id: 17, action: 'jitter_replication_window', reaction: 'back_off_selector' },
    { id: 18, action: 'boost_ws_rate', reaction: 'buffer_ws_backpressure' },
    { id: 19, action: 'prewarm_frontier_edge', reaction: 'pin_next_route_cache' },
    { id: 20, action: 'ignite_next_api', reaction: 'mirror_express_contract' },
    { id: 21, action: 'commit_guard_playbook', reaction: 'emit_full_audit' }
];

export const actions = actionReactionPairs.map(({ id, action, reaction }) => ({
    id,
    action,
    reactionHint: reaction,
    reactionEndpoint: `/api/reactions/${id}`
}));

export const reactions = actionReactionPairs.map(({ id, action, reaction }) => ({
    id,
    actionSource: action,
    reaction,
    status: 'paired'
}));

export function findAction(id) {
    return actions.find((entry) => entry.id === id);
}

export function findReaction(id) {
    return reactions.find((entry) => entry.id === id);
}
