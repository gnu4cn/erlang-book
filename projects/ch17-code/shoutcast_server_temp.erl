start_parallel_server(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, ..),
    %% create a song server -- this just knows about all our music
    PidSongServer = spawn(fun() -> songs() end),
    spawn(fun() -> par_connect(Listen, PidSongServer) end)..

%% spawn one of these processes per connection
par_connect(Listen, PidSongServer) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    %% when accept returns spawn a new process to
    %% wait for the next connection
    spawn(fun() -> par_connect(Listen, PidSongServer) end),
    inet:setopts(Socket, [{packet,0},binary,{nodelay,true},
                          {active,true}]),
    %% deal with the request
    get_request(Socket, PidSongServer, []).


%% wait for the TCP request
get_request(Socket, PidSongServer, L) ->
    receive
        {tcp, Socket, Bin} ->
            ... Bin contains the request from the client
            ... if the request is fragmented we call loop again ...
            ... otherwise we call
            .... got_request(Data, Socket, PidSongServer)
        {tcp_closed, Socket} ->
            ... this happens if the client aborts
            ... before it has sent a request (very unlikely)
    end.

%% we got the request -- send a reply
got_request(Data, Socket, PidSongServer) ->
    .. data is the request from the client ...
    .. analyze it ...
    .. we'll always allow the request ..
    gen_tcp:send(Socket, [response()]),
    play_songs(Socket, PidSongServer).

%% play songs forever or until the client quits
play_songs(Socket, PidSongServer) ->
    ... PidSongServer keeps a list of all our MP3 files
    Song = rpc(PidSongServer, random_song),
    ... Song is a random song ...
    Header = make_header(Song),
    ... make the header ...
    {ok, S} = file:open(File, [read,binary,raw]),
    send_file(1, S, Header, 1, Socket),
    file:close(S),
    play_songs(Socket, PidSongServer).

send_file(K, S, Header, OffSet, Socket) ->
    ... send the file in chunks to the client ...
    ... returns when the entire file is sent ...
    ... but exits if we get an error when writing to
    ... the sockets -- this happens if the client quits
