-module(c).
...

fonts_in(Str) ->
    X = a:make_text(Str),
    [F || {F,_} <- X].
