#!/usr/bin/env bash
set -euo pipefail

WORKDIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$WORKDIR"

COMPOSE_CMD=${COMPOSE_CMD:-docker compose}
ADMIN_USER=${ADMIN_USER:-admin}
ADMIN_PASSWORD=${ADMIN_PASSWORD:-'Mv@20217$'}
SERVICE_USER=${SERVICE_USER:-guard-producer}
SERVICE_PASSWORD=${SERVICE_PASSWORD:-'Mv@20217$'}
ZOOKEEPER_CONNECT=${ZOOKEEPER_CONNECT:-zookeeper-1:2181,zookeeper-2:2181,zookeeper-3:2181}

log() {
  printf '[Kafka SCRAM] %s\n' "$1"
}

configure_user() {
  local username=$1
  local password=$2
  log "Configuring SCRAM-SHA-512 creds for ${username}"
  $COMPOSE_CMD exec -T kafka kafka-configs \
    --zookeeper "$ZOOKEEPER_CONNECT" \
    --alter \
    --add-config "SCRAM-SHA-512=[password=${password},iterations=4096]" \
    --entity-type users \
    --entity-name "$username"
}

log "Ensuring ZooKeeper quorum and Kafka broker are running"
$COMPOSE_CMD up -d kafka

configure_user "$ADMIN_USER" "$ADMIN_PASSWORD"
configure_user "$SERVICE_USER" "$SERVICE_PASSWORD"

cat <<EOF

[Kafka SCRAM] Complete. Store these secrets safely:
  ADMIN_USER=${ADMIN_USER}
  ADMIN_PASSWORD=${ADMIN_PASSWORD}
  SERVICE_USER=${SERVICE_USER}
  SERVICE_PASSWORD=${SERVICE_PASSWORD}

Use these as KAFKA_USERNAME/KAFKA_PASSWORD env vars for clients (e.g., change-feed listener).
EOF
