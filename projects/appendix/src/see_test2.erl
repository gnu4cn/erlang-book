-module(see_test2).
-export([main/0]).

main() ->
    erlang:display({about_to_call,my_code}),
    2000 = my_code:double(1000),
    see:write("see_test2 worked\n").
