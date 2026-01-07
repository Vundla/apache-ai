-module(rocket_ws_http_handler).
-export([init/2]).

init(Req, _State) ->
    Body = <<"<!doctype html>\n<html>\n<head><meta charset=\"utf-8\"><title>Rocket Telemetry</title></head>\n<body>\n<h1>Rocket Telemetry</h1>\n<div id=\"log\"></div>\n<script>\nconst log = document.getElementById('log');\nconst ws = new WebSocket('ws://' + location.host + '/ws');\nws.onmessage = (ev) => {\n  const pre = document.createElement('pre');\n  pre.textContent = ev.data;\n  log.prepend(pre);\n};\n</script>\n</body>\n</html>\n">>,
    Req2 = cowboy_req:reply(200, #{"content-type" => "text/html"}, Body, Req),
    {ok, Req2, _State}.
