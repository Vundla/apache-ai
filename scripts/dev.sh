#!/usr/bin/env bash
set -euo pipefail

pushd "$(dirname "$0")/.." >/dev/null

echo "[compile]"
rebar3 compile

echo "[run] starting hidden node with apps..."
erl -pa _build/default/lib/*/ebin \
    -config config/sys.config \
    -args_file config/vm.args \
    -eval 'ok=application:ensure_all_started(rocket_core), ok=application:ensure_all_started(rocket_ws).'

popd >/dev/null
