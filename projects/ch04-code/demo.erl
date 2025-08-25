-module(demo).
-export([f/1, g/1]).

f(X) when (X == 0) or (1/X > 2) -> X.
g(X) when (X == 0) orelse (1/X > 2) -> X.
