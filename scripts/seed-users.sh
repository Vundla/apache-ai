#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"

echo "[users] Cockroach: create least-privilege users"
cat <<'SQL' | docker exec -i $(docker ps --format '{{.Names}}' | grep cockroach | head -n1) bash -lc "cockroach sql --insecure --host=localhost:26257"
CREATE USER IF NOT EXISTS observer_writer;
CREATE USER IF NOT EXISTS replicator;
GRANT SELECT, INSERT ON TABLE rocket_ai.engine_telemetry TO observer_writer;
GRANT SELECT ON TABLE rocket_ai.ai_insights TO observer_writer;
GRANT SELECT ON ALL TABLES IN DATABASE rocket_ai TO replicator;
SQL

echo "[users] Postgres: create least-privilege users"
docker exec -i $(docker ps --format '{{.Names}}' | grep postgres | head -n1) psql -U postgres -d rocket_ai <<'SQL'
DO $$ BEGIN
  CREATE ROLE observer_writer LOGIN PASSWORD 'observer_pass';
EXCEPTION WHEN duplicate_object THEN END $$;
DO $$ BEGIN
  CREATE ROLE replicator LOGIN PASSWORD 'replicator_pass';
EXCEPTION WHEN duplicate_object THEN END $$;
GRANT SELECT, INSERT ON TABLE telemetry_hypertable TO observer_writer;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO replicator;
SQL

echo "[users] Cassandra: create roles"
docker exec -i $(docker ps --format '{{.Names}}' | grep cassandra | head -n1) cqlsh -u cassandra -p cassandra <<'CQL'
CREATE ROLE IF NOT EXISTS observer_writer WITH PASSWORD = 'observer_pass' AND LOGIN = true;
CREATE ROLE IF NOT EXISTS replicator WITH PASSWORD = 'replicator_pass' AND LOGIN = true;
GRANT SELECT ON KEYSPACE rocket_ai TO replicator;
GRANT MODIFY ON KEYSPACE rocket_ai TO observer_writer;
CQL

echo "[users] CouchDB: add users (requires _users DB)"
echo "Use Fauxton or API to create 'observer_writer' and restrict db access."

echo "[users] done"
