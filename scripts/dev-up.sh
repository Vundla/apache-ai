#!/usr/bin/env bash
set -euo pipefail

echo "[dev] Starting databases via docker compose"
docker compose up -d

echo "[dev] To seed schemas: bash scripts/seed-schemas.sh"
echo "[dev] To run hidden observer: (cd apps/hidden_observer && npm install && npm start)"
