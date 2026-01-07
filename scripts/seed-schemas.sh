#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"

echo "[seed] Cockroach schema"
cat "$ROOT_DIR/config/schemas/cockroach.sql" | docker exec -i $(docker ps --format '{{.Names}}' | grep _cockroach_\|^cockroach$\|^cockroach-1$ | head -n1) bash -lc "cockroach sql --insecure --host=localhost:26257"

echo "[seed] Postgres schema"
docker exec -i $(docker ps --format '{{.Names}}' | grep _postgres_\|^postgres$ | head -n1) psql -U rocket -d rocket_ai < "$ROOT_DIR/config/schemas/postgres.sql" || \
docker exec -i $(docker ps --format '{{.Names}}' | grep _postgres_\|^postgres$ | head -n1) psql -U postgres -d rocket_ai < "$ROOT_DIR/config/schemas/postgres.sql" || true

echo "[seed] Cassandra schema"
docker exec -i $(docker ps --format '{{.Names}}' | grep _cassandra_\|^cassandra$ | head -n1) cqlsh -u cassandra -p cassandra < "$ROOT_DIR/config/schemas/cassandra.cql"

echo "[seed] done"
