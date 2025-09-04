-module(underscore_var).
-export([some_func/1]).

some_func(X) ->
    {P, _Q} = some_other_func(X),
    io:format("_Q = ~p~n", [_Q]),
    P.

some_other_func(X) ->
    {X, X * X}.
