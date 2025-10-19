-module(bad).

foo(A, B) ->
    bar(A, dothis(X), B),
    baz(Y, X).
