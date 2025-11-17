-module(see_test4).
-export([main/0]).

main() ->
    see:write("I will crash now\n"),
    1 = 2,
    see:write("This line will not be printed\n").
