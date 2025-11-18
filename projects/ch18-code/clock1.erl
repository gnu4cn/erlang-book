-module(clock1).
-export([start/1, current_time/0]).

start(Browser) ->
    Browser ! #{ cmd => fill_div, id => clock, txt => current_time() },
    running(Browser).

running(Browser) ->
    receive
        {Browser, #{ clicked => <<"stop">>} } ->
            idle(Browser)
    after 1000 ->
              Browser ! #{ cmd => fill_div, id => clock, txt => current_time() },
              running(Browser)
    end.

idle(Browser) ->
    receive
        {Browser, #{clicked => <<"start">>} } ->
            running(Browser)
    end.

current_time() ->
    {Hour,Min,Sec} = time(),
    list_to_binary(io_lib:format("~2.2.0w:~2.2.0w:~2.2.0w",
                                 [Hour,Min,Sec])).

