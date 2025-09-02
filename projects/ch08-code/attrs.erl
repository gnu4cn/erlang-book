-module(attrs).
-vsn("0.0.1").
-author({joe,armstrong}).
-purpose("example of attributes").
-export([fac/1, mysum/1]).
-import(lists, [map/2, sum/1]).


fac(1) -> 1;
fac(N) -> N * fac(N-1).

mysum([]) -> 0;
mysum(L)  -> 
    L1 = map(fun(X) -> 2 * X end, L),
    sum(L1).

