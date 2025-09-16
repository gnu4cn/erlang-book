-module(area_server1).
-export([loop/0, rpc/2]).
-define(PI, 3.14159).

rpc(Pid, Request) -> 
    Pid ! {self(), Request},
    receive
        Response -> Response
    end.

loop() -> 
    receive
        {From, {rectangle, Width, Ht}} ->
            From ! Width * Ht,
            loop();
        {From, {square, Side}} ->
            From ! Side * Side,
            loop();
        {From, {circle, Radius}} ->
            From ! ?PI * Radius * Radius,
            loop();
        {From, Other} ->
            From ! {error, Other},
            loop()
    end.
