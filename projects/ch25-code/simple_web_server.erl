-module(simple_web_server).
-export([start/0, start/1, init/2, terminate/3]).

start() ->                                                      %% Line 1
    start(8080).

start(Port) ->
    ok = application:start(crypto),                             %%      5
    {ok, _} = application:ensure_all_started(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),
    Dispatch = cowboy_router:compile(
                 [                                              %%     10
                  %% {URIHost, list(URIPath, Handler, Opts)}
                  {'_', [{'_', simple_web_server, []}]}
                 ]),
    cowboy:start_clear(my_simple_web_server,
                       [{port, Port}],                          %%     11
                       #{env => #{dispatch => Dispatch}}
                      ).

init(Req, State) ->                                                         %% Line 1
    Path = cowboy_req:path(Req),                                            %%      2
    Response = read_file(Path),                                             %%      3
    Req2 = cowboy_req:reply(200, #{                                         %%      4
                                   <<"content-type">> => <<"text/plain">>   %%      5
                                  }, Response, Req),                        %%      6
    {ok, Req2, State}.                                                      %%      7

read_file(Path) ->
    File = ["."|binary_to_list(Path)],
    case file:read_file(File) of
        {ok, Bin} -> Bin;
        _ -> ["<pre>cannot read:", File, "</pre>"]
    end.

terminate(_Reason, _Req, _State) ->
    ok.
