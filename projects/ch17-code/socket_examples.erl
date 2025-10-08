-module(socket_examples).
-export([nano_get_url/0, nano_get_url/1, nano_client_eval/1]).
-import(lists, [reverse/1]).

nano_get_url() ->
    nano_get_url("httpforever.com").

nano_get_url(Host) ->
    {ok, Socket} = gen_tcp:connect(Host, 80, [binary, {packet, 0}]),    %% 1
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),                %% 2
    receive_data(Socket, []).

receive_data(Socket, SoFar) ->
    receive
        {tcp,Socket,Bin} ->                                             %% 3
            receive_data(Socket, [Bin|SoFar]);
        {tcp_closed, Socket} ->                                         %% 4
            list_to_binary(reverse(SoFar))                              %% 5
    end.

start_nano_server() ->
    {ok, Listen} = gen_tcp:listen(2345, [binary, {packet, 4},   %% 1
                                         {reuseaddr, true},
                                         {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),                      %% 2
    gen_tcp:close(Listen),                                      %% 3
    loop(Socket).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            io:format("Server received binary = ~p~n", [Bin]),
            Str = binary_to_term(Bin),                          %% 4
            io:format("Server (unpacked) ~p~n", [Str]),
            Reply = lib_misc:string2value(Str),                 %% 5
            io:format("Server replying = ~p~n", [Reply]),
            gen_tcp:send(Socket, term_to_binary(Reply)),        %% 6
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.

nano_client_eval(Str) ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345,
                                  [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
        {tcp,Socket,Bin} ->
            io:format("Client received binary = ~p~n", [Bin]),
            Val = binary_to_term(Bin),
            io:format("Client result = ~p~n", [Val]),
            gen_tcp:close(Socket)
    end.
