-module(simple_web_server).
-export([start/0, start/1, init/3, handle/2, terminate/3]).

start(Port) ->                                                  %% Line 1
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    N_acceptors = 10,                                           %%      5
    Dispatch = cowboy_router:compile(
                 [
                  %% {URIHost, list(URIPath, Handler, Opts)}
                  {'_', [{'_', simple_web_server, []}]}
                 ]),                                             %%     10
    cowboy:start_http(my_simple_web_server,
                      N_acceptors,
                      [{port, Port}],
                      [{env, [{dispatch, Dispatch}]}]
                     ).                                         %%     15


init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->                                       %% Line 1
    {Path, Req1} = cowboy_req:path(Req),                    %%      2
    Response = read_file(Path),                        %%      3
    {ok, Req2} = cowboy_req:reply(200, [], Response, Req1), %%      4
    {ok, Req2, State}.                                      %%      5

read_file(Path) ->
    File = ["."|binary_to_term(Path)],
    case file:read_file(File) of
        {ok, Bin} -> Bin;
        _ -> ["<pre>cannot read:", File, "</pre>"]
    end.

terminate(_Reason, _Req, _State) ->
    ok.

start() ->
    start(8080).
