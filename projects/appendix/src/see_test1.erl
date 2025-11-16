-module(see_test1).
-export([main/0]).

main() ->
    see:write("HELLO WORLD\n"),
    see:write(integer_to_list(see:modules_loaded()-8) ++ " modules loaded\n").
