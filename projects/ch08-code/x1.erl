-module(x1).
-export([square/1, double/1]).


square(X) -> X * X.
double(L) -> lists:map(fun square/1, L).
