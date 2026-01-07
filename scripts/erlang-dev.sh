#!/usr/bin/env bash
set -euo pipefail

if ! command -v rebar3 >/dev/null; then
  echo "[erlang] rebar3 not found. Please install rebar3 in WSL PATH." >&2
  exit 1
fi

export ERL_AFLAGS="-hidden -setcookie rocket_ai_cookie"

echo "[erlang] compiling"
rebar3 compile

echo "[erlang] starting shell with apps"
rebar3 shell --apps rocket_core,rocket_ws --name rocket@127.0.0.1 --setcookie rocket_ai_cookie -config config/sys.config -args_file config/vm.args
