#!/usr/bin/env bash
set -euo pipefail

# Simple dev TLS cert generator (self-signed). For production, use a proper CA.

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
TLS_DIR="$ROOT_DIR/config/tls"
mkdir -p "$TLS_DIR"

echo "[tls] Generating CA"
openssl genrsa -out "$TLS_DIR/ca.key" 4096
openssl req -x509 -new -nodes -key "$TLS_DIR/ca.key" -sha256 -days 3650 \
  -subj "/CN=Rocket Dev CA" -out "$TLS_DIR/ca.crt"

gen_cert() {
  local name=$1 cn=$2
  shift 2
  local sans="$@"
  
  openssl genrsa -out "$TLS_DIR/${name}.key" 2048
  openssl req -new -key "$TLS_DIR/${name}.key" -out "$TLS_DIR/${name}.csr" -subj "/CN=${cn}"
  
  if [[ -n "$sans" ]]; then
    openssl x509 -req -in "$TLS_DIR/${name}.csr" -CA "$TLS_DIR/ca.crt" -CAkey "$TLS_DIR/ca.key" -CAcreateserial \
      -out "$TLS_DIR/${name}.crt" -days 825 -sha256 -extfile <(printf "subjectAltName=${sans}")
  else
    openssl x509 -req -in "$TLS_DIR/${name}.csr" -CA "$TLS_DIR/ca.crt" -CAkey "$TLS_DIR/ca.key" -CAcreateserial \
      -out "$TLS_DIR/${name}.crt" -days 825 -sha256
  fi
}

echo "[tls] Generating service certs"
gen_cert cockroach cockroach
gen_cert postgres postgres
gen_cert couchdb couchdb
gen_cert cassandra cassandra
# Reissue cassandra cert with SAN for dev (DNS:cassandra, IP:127.0.0.1)
openssl req -new -key "$TLS_DIR/cassandra.key" -out "$TLS_DIR/cassandra.csr" -subj "/CN=cassandra" -addext "subjectAltName=DNS:cassandra,IP:127.0.0.1"
openssl x509 -req -in "$TLS_DIR/cassandra.csr" -CA "$TLS_DIR/ca.crt" -CAkey "$TLS_DIR/ca.key" -CAcreateserial \
  -out "$TLS_DIR/cassandra.crt" -days 825 -sha256 -extensions v3_req -extfile <(printf "[v3_req]\nsubjectAltName=DNS:cassandra,IP:127.0.0.1\n")
gen_cert kafka kafka

echo "[tls] Generating client certs"
gen_cert client_observer observer
gen_cert client_replicator replicator

echo "[tls] Done: see $TLS_DIR"
