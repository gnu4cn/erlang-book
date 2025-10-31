-module(example_mod).
-export([test_func/2]).

test_func(Arg1, TupleModule) ->
    io:format("Arg1: ~p, TupleModule: ~p~n", [Arg1, TupleModule]).
