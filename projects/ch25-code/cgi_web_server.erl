-module(cgi_web_server).
-export([start/0, start/1, init/2, terminate/3, start_from_shell/1]).

start() ->
    start(8080).

start(Port) ->
    {ok, _} = application:ensure_all_started(cowboy),
    Dispatch = cowboy_router:compile(
                 [                                              %%     10
                  %% {URIHost, list(URIPath, Handler, Opts)}
                  {'_', [{'_', cgi_web_server, []}]}
                 ]),
    cowboy:start_clear(cgi_web_server,
                       [{port, Port}],                          %%     11
                       #{env => #{dispatch => Dispatch}}
                      ).

init(Req, State) ->
    Path = cowboy_req:path(Req),
    handle(Path, Req, State).

handle(<<"/cgi">>, Req, State) ->
    Qstr = cowboy_req:parse_qs(Req),
    %% io:format("Qstr: ~p~n", [Qstr]),
    {ok, Bin, Req1} = cowboy_req:read_body(Req),
    {struct, Qbody} = mochijson2:decode(Bin),
    Response = call(Qstr, Qbody),
    Json = mochijson2:encode(Response),
    Req2 = cowboy_req:reply(200, #{
                                   <<"content-type">> => <<"text/json">>
                                  }, Json, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req2),
    {ok, Req3, State};
handle(Path, Req, State) ->
    Response = read_file(Path),
    Req2 = cowboy_req:reply(200, #{
                                   <<"content-type">> => <<"text/html">>
                                  }, Response, Req),
    {ok, Req2, State}.

call([{<<"mod">>,MB},{<<"func">>,FB}], X) ->
    Mod = list_to_atom(binary_to_list(MB)),
    Func = list_to_atom(binary_to_list(FB)),
    apply(Mod, Func, [X]).

read_file(Path) ->
    File = ["."|binary_to_list(Path)],
    case file:read_file(File) of
        {ok, Bin} -> Bin;
        _ -> ["<pre>cannot read:", File, "</pre>"]
    end.

terminate(_Reason, _Req, _State) ->
    ok.

start_from_shell([PortAsAtom]) ->
    PortAsInt = list_to_integer(atom_to_list(PortAsAtom)),
    start(PortAsInt).
