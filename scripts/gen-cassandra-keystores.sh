#!/usr/bin/env bash
set -euo pipefail

# Generates JKS keystore/truststore for Cassandra client TLS using local CA and server cert.

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")"/.. && pwd)"
TLS_DIR="$ROOT_DIR/config/tls"
JKS_DIR="$ROOT_DIR/config/cassandra/jks"
mkdir -p "$JKS_DIR"

if ! command -v keytool >/dev/null; then
  echo "keytool not found. Install openjdk (e.g., sudo apt install openjdk-17-jre-headless)." >&2
  exit 1
fi

CA_CRT="$TLS_DIR/ca.crt"
SERVER_CRT="$TLS_DIR/cassandra.crt"
SERVER_KEY="$TLS_DIR/cassandra.key"

echo "[cassandra] creating PKCS12 keystore from server cert/key"
openssl pkcs12 -export -in "$SERVER_CRT" -inkey "$SERVER_KEY" -out "$JKS_DIR/keystore.p12" -name cassandra -passout pass:changeit

echo "[cassandra] importing PKCS12 into JKS keystore"
keytool -importkeystore -srckeystore "$JKS_DIR/keystore.p12" -srcstoretype PKCS12 -srcstorepass changeit \
  -destkeystore "$JKS_DIR/keystore.jks" -deststoretype JKS -deststorepass changeit -alias cassandra -noprompt

echo "[cassandra] creating truststore with CA"
keytool -import -file "$CA_CRT" -alias rocket-ca -keystore "$JKS_DIR/truststore.jks" -storepass changeit -noprompt

ls -l "$JKS_DIR"
echo "[cassandra] JKS files ready at $JKS_DIR"
