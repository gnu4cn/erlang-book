receive
    Pattern1 [when Guard1] ->
        Expression1;
    Pattern2 [when Guard2] ->
        Expression2;
    ...
end


loop() ->
    receive
        {From, ...} ->
            From ! {self(), ...},
            loop()
            ...
    end.


receive
    Pattern1 [when Guard1] ->
        Expressions1;
    Pattern2 [when Guard2] ->
        Expressions2;
    ...
after Time ->
          Expressions
end
