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


receive
    Pattern1 [when Guard1] ->
        Expressions1;
    Pattern2 [when Guard2] ->
        Expressions2;
    ...
after
    Time ->
        ExpressionsTimeout
end



loop() ->
    receive
        {From, {rectangle, Width, Ht}} ->
            From ! {self(), Width*Ht},
            loop(),
            someOtherFunc();
        {From, {circle, R}} ->
            From ! {self(), 3.14159 * R * R},
            loop();
            ...
    end
end
