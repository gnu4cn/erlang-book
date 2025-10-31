-module(counter).
-export([bump/2, read/1, test/0]).
-compile(tuple_calls).

bump(N, {counter,K}) -> {counter, N + K}.

read({counter, N}) -> N.

test() ->
    C = {counter,2},
    C:read().
