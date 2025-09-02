-module(x2).
-export([double/1]).


double(L) -> lists:map(fun x1:square/1, L).
