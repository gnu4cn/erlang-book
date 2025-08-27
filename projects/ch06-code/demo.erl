try FuncOrExpressionSeq of
    Pattern1 [when Guard1] -> Expressions1;
    Pattern2 [when Guard2] -> Expressions2;
    ...
catch
ExceptionType1: ExPattern1 [when ExGuard1] -> ExExpressions1;
ExceptionType2: ExPattern2 [when ExGuard2] -> ExExpressions2;
...
after
    AfterExpressions
end


f(...) ->
    ...
    try ... end,
    ...
    ...
