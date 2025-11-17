-module(see_test3).
-export([main/0]).
-import(see, [read/0, write/1]).

main() -> loop().

loop() ->
    case read() of
        eof ->
            true;
        {ok, X} ->
            write([X]),
            loop()
    end.
