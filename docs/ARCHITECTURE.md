# Conscious Rocket AI — Architecture & Implementation Guide

This guide captures the full backend plan so work can resume instantly after interruptions. It targets **100% backend coverage** (data layer, BEAM runtime, Kafka security guards) and explains how to activate every subsystem.

## 1. System Layers

| Layer | Technology | Purpose |
| --- | --- | --- |
| Hidden Compute Core | Erlang/OTP (-hidden nodes, `pg`, Cowboy) | Real-time telemetry generation + WebSocket streaming. |
| Secure Engines | CockroachDB, Cassandra, Postgres, CouchDB/PouchDB | Strongly consistent SQL + time-series + analytics + offline replicas. |
| Observers & Guards | Node.js hidden observer, change-feed listener, Kafka security topics | Offline-first telemetry buffering, encrypted replication, anomaly feeds. |
| Coordination Quorum | Apache ZooKeeper (3-node ensemble) | Witness-ledger for Kafka guard elections, audit trails, and offline guard activations. |
| API Mesh | Erlang/Cowboy, Express service, Next.js API routes | 21 paired action↔reaction endpoints for telemetry orchestration across stacks. |
| Security Surface | TLS (self-signed for dev), SCRAM/SASL (Kafka), least-privilege DB users | Zero-trust comms + least privilege + encrypted storage. |
| Frontline Visualization | Next.js+Solid.js (planned) | Lightweight dashboards fed from WebSocket channel. |

## 2. Hidden BEAM Backend

- **Apps**: `rocket_core` (synthetic telemetry) + `rocket_ws` (Cowboy WebSocket + test HTML page).
- **Hidden features**:
  - `config/vm.args` sets `-hidden` plus fixed cookie & port range (9100-9105).
  - `scripts/erlang-dev.sh` exports `ERL_AFLAGS` and launches both apps via `rebar3 shell`.
  - `rocket_core_telemetry` broadcasts via `pg` instead of global process registry (harder to discover).
  - `rocket_ws_handler` pushes JSON frames to `/ws`; default page at `/` automatically connects.
- **Run**:

```bash
cd /home/vundla/apache-ai
bash scripts/erlang-dev.sh
# visit http://localhost:8080 (ws://localhost:8080/ws)
```

## 3. Database & Storage Stack

1. **TLS material**
   - `scripts/gen-certs.sh` (generic services).
   - `scripts/cockroach_tls_setup.sh` generates Cockroach CA/node/client certs (secure mode).
   - `scripts/gen-cassandra-keystores.sh` converts PEM → JKS for Cassandra client encryption.

2. **Containers** (`docker-compose.yml`)
   - CockroachDB: secure mode, certs mounted at `/certs`.
   - Cassandra: custom `config/cassandra/cassandra.yaml` (full base config with TLS block) + `/certs/jks` mount.
   - Postgres: TLS on, SCRAM, custom `pg_hba.conf` & `postgresql.conf`.
   - CouchDB: SSL enabled, `require_valid_user=true`.
   - Kafka: Confluent broker (`kafka`) bound to the ZooKeeper quorum, TLS 1.3 listeners on `9093`, SCRAM-SHA-512 inter-broker auth, per-node volume for audit retention.

3. **Least-privilege users**
   - `scripts/seed-users.sh` creates `observer_writer` / `replicator` roles in Cockroach, Postgres, Cassandra; guide instructs to add CouchDB `_users` docs.

4. **Schema ownership**
   - All schema SQL/CQL files include header referencing owners: Apache, Capaciti, ALX (see `config/schemas`).

## 4. Kafka Security Guards & ZooKeeper Witnesses

- **Coordination quorum**: `docker-compose` now ships `zookeeper-1`, `zookeeper-2`, `zookeeper-3` (Confluent images) with a shared `ZOOKEEPER_SERVERS` string so they form a majority-voting ensemble. `zookeeper-1` exposes port `2181` for host diagnostics, while the other replicas stay headless—ideal “background under-dog guards.”
- **Audit activation**: Every ACL/SCRAM change that Kafka will make later is first durably written to ZooKeeper znodes, giving us the 100% review/audit guarantee before brokers ever emit telemetry.
- **Offline replicas**: Each node writes to its own Docker volume, so any two surviving nodes can elect a leader and keep the guard ledger readable even if Kafka or one ZooKeeper replica is offline.
  - Verify quorum locally:

      ```bash
      docker compose up -d zookeeper-1 zookeeper-2 zookeeper-3
      echo stat | nc localhost 2181
      ```

      Expect `Mode: follower` or `Mode: leader` to confirm ensemble health.
- **Kafka broker (now running)**: `docker-compose` starts `kafka` automatically; it boots with TLS 1.3, SCRAM-SHA-512 inter-broker auth, ACL authorizer, and points at `zookeeper-1:2181,zookeeper-2:2181,zookeeper-3:2181`.
  - Sanity-test the TLS socket:

      ```bash
      docker compose up -d kafka  # pulls ZooKeeper deps automatically
      openssl s_client -connect localhost:9093 -tls1_3 -showcerts <<< ""
      ```

      Expect a successful TLS handshake with the broker certificate chain.
- **SCRAM credential rollout**: `scripts/configure-kafka-scram.sh` provisions both the `admin` super user and a `guard-producer` service account directly inside ZooKeeper so credentials survive broker restarts. Override secrets per run:

```bash
ADMIN_PASSWORD='admin-super-secret' \
SERVICE_PASSWORD='guard-telemetry-secret' \
bash scripts/configure-kafka-scram.sh
```

The script ensures Kafka is up, writes SCRAM-SHA-512 hashes (`iterations=4096`), and prints the credentials you must copy into client `.env` files (`KAFKA_USERNAME`, `KAFKA_PASSWORD`).
If you omit overrides, both accounts default to the shared password `Mv@20217$` (short-lived for dev only).

- **Guard pipeline**:
   1. Kafka broker container (running) with TLS certs from `config/tls` and SCRAM users anchored in ZooKeeper.
   2. Topic naming: `security-guard-feed`, `telemetry-alerts`, `ai-patterns`.
   3. Producer: `apps/change_feed_listener` (long-polls CouchDB `_changes`, publishes minimal metadata).
   4. Consumer (future): Erlang or Go service subscribed to `security-guard-feed` to trigger AI guards.

## 5. Hidden Observer Pipeline

- Location: `apps/hidden_observer` (Node.js + PouchDB LevelDB + AES-256-GCM).
- Features:
  - Encrypts every document before storage (`ENCRYPTION_KEY_HEX`).
  - Selector-based replication (only telemetry docs with encrypted payload + metadata) with jitter + batch sizing (reduced network signature).
  - Works offline indefinitely; only syncs when CouchDB reachable.
- Run:

```bash
cd /home/vundla/apache-ai/apps/hidden_observer
npm install
npm start
```

## 6. CouchDB Change-Feed Listener

- Location: `apps/change_feed_listener` (axios + kafkajs).
- Behavior: long-poll `_changes`, optionally publishes to Kafka topic defined in `.env` (`KAFKA_BOOTSTRAP`, `KAFKA_TOPIC`).
- Hardened output: only minimal metadata (`id`, `seq`, `type`, `ts`).

## 7. Cassandra TLS Completion Plan

1. **Merge config**: copy `config/cassandra/cassandra.full.yaml` from container and patch in:

    ```yaml
    client_encryption_options:
       enabled: true
       optional: false
       keystore: /certs/jks/keystore.jks
       keystore_password: changeit
       truststore: /certs/jks/truststore.jks
       truststore_password: changeit
       require_client_auth: false
    server_encryption_options:
       internode_encryption: none
    ```

   Save as `config/cassandra/cassandra.yaml` (complete file, not fragment).

1. **Restart**:

```bash
cd /home/vundla/apache-ai
docker compose up -d cassandra
```

1. **Test** (`apps/cassandra_test`):

```bash
cd apps/cassandra_test
npm install --no-audit --no-fund
node src/index.js
```

- If you see `ERR_TLS_CERT_ALTNAME_INVALID`, regenerate service cert with SAN including `127.0.0.1` or connect via hostname present in cert (e.g., `cassandra`).

## 8. Removing sudo from Docker

Option A (recommended):

```bash
sudo groupadd docker 2>/dev/null || true
sudo usermod -aG docker "$USER"
newgrp docker
docker run --rm hello-world
```

Option B: Docker Desktop WSL integration (toggle in GUI).

Option C: Rootless Docker (needs `uidmap`, `dbus-user-session`; run `dockerd-rootless-setuptool.sh install`).

## 9. Resume Checklist (Backend 100%)

1. **TLS assets**: `scripts/gen-certs.sh`, `scripts/cockroach_tls_setup.sh`, `scripts/gen-cassandra-keystores.sh`.
2. **Start stack**: `docker compose up -d` (requires non-sudo Docker).
3. **Verify ZooKeeper quorum**:

   ```bash
   docker compose up -d zookeeper-1 zookeeper-2 zookeeper-3
   echo ruok | nc localhost 2181  # expect imok
   ```

4. **Seed schemas + users**:

   ```bash
   bash scripts/seed-schemas.sh
   bash scripts/seed-users.sh
   ```

5. **Verify Cockroach TLS**: `bash scripts/test-cockroach-tls.sh`.
6. **Verify Cassandra TLS**: `cd apps/cassandra_test && node src/index.js`.
7. **Verify Kafka TLS guard**:

   ```bash
   docker compose up -d kafka
   openssl s_client -connect localhost:9093 -tls1_3 -showcerts <<< ""
   ```

   Confirm the TLS handshake succeeds and the broker certificate matches `kafka`.
8. **Provision Kafka SCRAM users**:

   ```bash
   ADMIN_PASSWORD='admin-super-secret' \
   SERVICE_PASSWORD='guard-telemetry-secret' \
   bash scripts/configure-kafka-scram.sh
   ```

   Record the emitted credentials securely; clients should export them as `KAFKA_USERNAME`/`KAFKA_PASSWORD`.
9. **Run hidden observer + change-feed listener**.
10. **Run Erlang hidden backend**.
11. **Future**: point change-feed listener to TLS bootstrap with SCRAM creds, add AI security consumer, expand ACL coverage.

## 10. Notes & Next Targets

- Wire Kafka clients (change-feed producer + future guard consumer) to the TLS/SCRAM broker and define ACLs per topic.
- TLS SAN fix for Cassandra certs (`config/tls/cassandra.crt`) so 127.0.0.1 is trusted.
- Extend README with quick-reference commands (use sections above).
- Add infra-as-code (e.g., Ansible) once manual process is stable.

Keep this guide open; after any interruption resume from §9. Resetting the backend to 100% simply means ensuring every step above is green before moving forward.

## 11. API Mesh — 21 Actions, 21 Reactions

- **Erlang/Cowboy core**: `rocket_ws` now exposes JSON endpoints guaranteed to emit the 21 action/reaction pairs:
  - `GET /api/actions` → full catalog with `reaction_endpoint` for each action.
  - `GET /api/actions/:id` and `GET /api/reactions/:id` → detail views with strict validation.
  - The catalogue lives inside `rocket_ws_api_handler.erl`, ensuring telemetry events and WebSockets can query the same contract without leaving the BEAM node.

- **Express mirror** (`apps/action_api`): lightweight Node service mirroring the Cowboy contract for downstream consumers or integration tests.

   ```bash
   cd apps/action_api
   npm install
   npm run dev
   curl http://localhost:7070/api/actions/5
   ```

   Includes an extra `POST /api/actions/:id/trigger` helper so tooling can simulate “action causes reaction” workflows.

- **Next.js API layer** (`apps/next_guard`): ships `pages/api/actions/*` and `pages/api/reactions/*` routes backed by the same catalog so the future dashboard can deploy serverless endpoints without duplicating logic.

   ```bash
   cd apps/next_guard
   npm install
   npm run dev
   curl http://localhost:3000/api/reactions/21
   ```

- **21-by-design guarantee**: all three stacks source from synchronized catalogs, so “every action has a reaction” is verifiable regardless of which runtime responds. Clients can health-check by expecting `total: 21` from any `/api/actions` response.
