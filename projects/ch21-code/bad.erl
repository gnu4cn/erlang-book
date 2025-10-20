-module(bad).

foo(X, L) ->
    lists:map(fun(X) -> 2*X end, L).
