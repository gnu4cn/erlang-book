-module(attrs).
-vsn("0.0.1").
-author({joe,armstrong}).
-purpose("example of attributes").
-export([fac/1]).


fac(1) -> 1;
fac(N) -> N * fac(N-1).
