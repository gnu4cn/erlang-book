-module(b).
...

do_this() ->
    X = a:make_text("hello world"),
    {W, H} = a:bounding_box(X)
