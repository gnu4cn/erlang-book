-module(counter).
-export([bump/2, read/1, test/0]).
-compile(tuple_calls).

bump(N, {counter,K}) -> {counter, N + K}.

read({counter, N}) -> N.

test() ->
    C = {counter,2},
    io:format("C: ~p~n", [C]),
    C:read(),
    C1 = C:bump(3),
    io:format("C1: ~p~n", [C1]),
    C1:read().
