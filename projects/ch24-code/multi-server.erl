-module(multi_server).                                      %% Line 1
-export([start/0]).

start() -> spawn(fun() -> multi_server() end).
                                                            %%      5
multi_server() ->
    receive
        {_Pid, {email, _From, _Subject, _Text} = Email} ->
            {ok, S} = file:open("mbox", [write,append]),
            io:format(S, "~p.~n", [Email]),                 %%     10
            file:close(S);
        {_Pid, {im, From, Text}} ->
            io:format("Msg (~s): ~s~n", [From, Text]);
        {Pid, {get, File}} ->
            Pid ! {self(), file:read_file(File)};           %%     15
        Any ->
            io:format("multi server got: ~p~n", [Any])
    end,
    multi_server().
