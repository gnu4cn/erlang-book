-module(socket_examples).
-export([nano_get_url/0, nano_get_url/2]).
%% -import(lists, [reverse/1]).

nano_get_url() ->
    nano_get_url("www.google.com", "/").

nano_get_url(Host, Path) ->
    {ok, Socket} = gen_tcp:connect(Host, 443, [{active, false}]),
    ssl:start(),
    {ok, TLSSocket} = ssl:connect(Socket, [{verify, verify_peer}, {cacerts, public_key:cacerts_get()}, {server_name_indication, Host}]),

    Request = io_lib:format("GET ~s HTTP/1.1\r\nHost: ~s\r\nConnection: close\r\n\r\n", [Path, Host]),
    ssl:send(TLSSocket, list_to_binary(Request)),
    {ok, Data} = ssl:recv(TLSSocket, 0),
    ssl:close(TLSSocket),
    Data.
