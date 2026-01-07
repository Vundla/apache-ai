#!/usr/bin/env bash
set -euo pipefail

echo "[cockroach] testing secure SQL connection"
CID=$(docker ps --format '{{.Names}}' | grep cockroach | head -n1)
if [[ -z "$CID" ]]; then
  echo "cockroach container not running" >&2
  exit 1
fi

docker exec -it "$CID" bash -lc "cockroach sql --certs-dir=/certs -e 'SHOW DATABASES;'"
