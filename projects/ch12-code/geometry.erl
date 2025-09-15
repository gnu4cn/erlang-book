-module(geometry).
-export([area/1]).
-define(PI, 3.14159).

area({rectangle, Width, Height}) -> Width * Height;
area({circle, Radius})	         -> ?PI * Radius * Radius;
area({square, Side})			 ->	Side * Side.
