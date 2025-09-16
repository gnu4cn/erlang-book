-module(area_server_final).
-export([loop/0, area/2, start/0]).
-define(PI, 3.14159).


start() -> spawn(area_server_final, loop, []).

area(Pid, What) -> rpc(Pid, What).

rpc(Pid, Request) -> 
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

loop() -> 
    receive
        {From, {rectangle, Width, Ht}} ->
            From ! {self(), {Width * Ht, From}},
            loop();
        {From, {square, Side}} ->
            From ! {self(), {Side * Side, From}},
            loop();
        {From, {circle, Radius}} ->
            From ! {self(), {?PI * Radius * Radius, From}},
            loop();
        {From, Other} ->
            From ! {self(), {{error, Other}, From}},
            loop()
    end.
