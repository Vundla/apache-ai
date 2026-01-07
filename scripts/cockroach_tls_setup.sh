#!/usr/bin/env bash
set -euo pipefail

# Generate CockroachDB CA, node, and client certs using the official image.

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
CERTS_DIR="$ROOT_DIR/config/cockroach/certs"
mkdir -p "$CERTS_DIR"

IMG="cockroachdb/cockroach:v24.3.1"

echo "[cockroach] creating CA"
docker run --rm -v "$CERTS_DIR":/certs "$IMG" cert create-ca --certs-dir=/certs --ca-key=/certs/ca.key

echo "[cockroach] creating node cert (localhost)"
docker run --rm -v "$CERTS_DIR":/certs "$IMG" cert create-node localhost 127.0.0.1 cockroach \
  --certs-dir=/certs --ca-key=/certs/ca.key

echo "[cockroach] creating client certs (root, observer_writer)"
docker run --rm -v "$CERTS_DIR":/certs "$IMG" cert create-client root --certs-dir=/certs --ca-key=/certs/ca.key
docker run --rm -v "$CERTS_DIR":/certs "$IMG" cert create-client observer_writer --certs-dir=/certs --ca-key=/certs/ca.key

ls -l "$CERTS_DIR"
echo "[cockroach] certs ready at $CERTS_DIR"
