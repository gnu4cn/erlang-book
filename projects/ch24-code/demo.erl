web_server(Client) ->
    receive
        {Client, {get, Page}} ->
            case file:read(Page) of
                {ok, Bin} ->
                    Client ! {self(), {data, Bin}};
                {error, _} ->
                    Client ! {self(), error}
            end,
            web_server(Client)
    end.

send1(Term) -> encrypt(compress(term_to_binary(Term))).

receive1(Bin) -> binary_to_term(decompress(decrypt(Bin))).


send_code(Mod, Func, Args) ->
    encrypt(compress(term_to_binary({Mod, Func, Args}))).

receive_code(Bin) ->
    {Mod, Func, Args} = binary_to_term(decompress(decrypt(Bin))),
    apply(Mod, Func, Args).

