-module(tuple_mod).
-compile(tuple_calls).
-export([test/0]).

test() ->
    C = {counter,2},
    C:read().
