-module(area_server2).
-export([loop/0, rpc/2]).
-define(PI, 3.14159).

rpc(Pid, Request) -> 
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

loop() -> 
    receive
        {From, {rectangle, Width, Ht}} ->
            From ! {self(), Width * Ht},
            loop();
        {From, {square, Side}} ->
            From ! {self(), Side * Side},
            loop();
        {From, {circle, Radius}} ->
            From ! {self(), ?PI * Radius * Radius},
            loop();
        {From, Other} ->
            From ! {self(), {error, Other}},
            loop()
    end.
