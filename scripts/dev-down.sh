#!/usr/bin/env bash
set -euo pipefail

echo "[dev] Stopping and removing docker compose stack"
docker compose down -v
