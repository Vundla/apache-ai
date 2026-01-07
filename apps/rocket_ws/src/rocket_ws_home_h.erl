-module(rocket_ws_home_h).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req0, _State) ->
    Html = <<
        "<!doctype html>\n"
        "<html><head><meta charset=\"utf-8\"><title>Rocket Live View</title>\n"
        "<style>body{font-family:system-ui;margin:2rem}#s{font-family:monospace;white-space:pre}</style>\n"
        "</head><body>\n"
        "<h1>Rocket Telemetry</h1>\n"
        "<div id=\"s\">Connecting...</div>\n"
        "<script>\n"
        "const s=document.getElementById('s');\n"
        "const ws=new WebSocket((location.protocol==='https:'?'wss':'ws')+'://'+location.host+'/ws');\n"
        "ws.onopen=()=>s.textContent='Connected. Waiting for telemetry...';\n"
        "ws.onmessage=(e)=>{s.textContent=e.data+'\n'+s.textContent.slice(0,2000)};\n"
        "ws.onclose=()=>s.textContent='Disconnected';\n"
        "</script>\n"
        "</body></html>\n"
    >>,
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/html; charset=utf-8">>
    }, Html, Req0),
    {ok, Req, undefined}.
