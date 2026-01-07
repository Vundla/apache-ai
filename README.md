# Conscious Rocket AI Observer — Backend & Replicas

This repo scaffolds the backend data layer and an offline-first "hidden observer" service for a lightweight, fault-tolerant rocket AI. It prioritizes encrypted storage, minimal network footprint, and resilient replication.

Key points:

- CockroachDB as primary, with Cassandra, Postgres, and CouchDB/PouchDB as replicas.
- Offline hidden observers buffer locally (PouchDB LevelDB) and sync selectively to CouchDB.
- End-to-end encryption (AES-256-GCM) for observer-stored data before any replication.
- Minimal network surface: filtered replication, randomized sync intervals, and backoff.

Ownership of schemas: Apache, Capaciti, ALX.

## Quick start (dev)

1) Copy env and start databases

```bash
cp config/.env.example .env
docker compose up -d
```

1) Seed schemas (Cockroach/Postgres/Cassandra)

```bash
bash scripts/seed-schemas.sh
```

1) Start the hidden observer (offline-first)

```bash
pushd apps/hidden_observer
npm install
npm start
popd
```

Notes:

- Dev compose uses non-TLS DB endpoints for simplicity. Enable TLS in production.
- Replication only ships redacted/metadata docs to CouchDB; encrypted payloads remain opaque.

# Rocket AI Observer (Backend Preview)

This is a minimal Erlang/OTP umbrella backend providing:

- Hidden BEAM node configuration (-hidden) for stealth clustering
- Cowboy WebSocket endpoint for live telemetry at <http://localhost:8080/>
- Periodic telemetry generator broadcasting to all WebSocket clients via pg

## Prerequisites (WSL)

- Erlang/OTP 24+ with rebar3 in PATH

## Run (dev)

$ chmod +x scripts/dev.sh
$ ./scripts/dev.sh

Then open <http://localhost:8080> to see live telemetry.

## Notes

- This is a backend preview; the AI observer, security guards, and databases will be integrated next.
- WebSocket path: /ws. Home page serves a tiny test client.

## Hidden Node

Configured in config/vm.args with -hidden and a demo cookie. Adjust the cookie for your cluster.
