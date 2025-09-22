-module(kvs).	                                        %% Line 1
-export([start/0, store/2, lookup/1]).

start() -> register(kvs, spawn(fun() -> loop() end)).
	                                                    %%	    5
store(Key, Value) -> rpc({store, Key, Value}).

lookup(Key) -> rpc({lookup, Key}).

rpc(Q) ->	                                            %%	   10 
    kvs ! {self(), Q},
    receive
        {kvs, Reply} -> Reply
    end.
	                                                    %%	   15

loop() -> 
    receive
        {From, {store, Key, Value}} ->
            put(Key, {ok, Value}),	                    %%	   20
            From ! {kvs, true},
            loop();
        {From, {lookup, Key}} ->
            From ! {kvs, get(Key)},
            loop()	                                    %%	   25
    end.
