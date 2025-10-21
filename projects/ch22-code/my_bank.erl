-module(my_bank).
-behaviour(gen_server).
-export([start/0]).
-export([stop/0, new_account/1, deposit/2, withdraw/2]).
-define(SERVER, ?MODULE).

start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop()  -> gen_server:call(?MODULE, stop).

new_account(Who)      -> gen_server:call(?MODULE, {new, Who}).
deposit(Who, Amount)  -> gen_server:call(?MODULE, {add, Who, Amount}).
withdraw(Who, Amount) -> gen_server:call(?MODULE, {remove, Who, Amount}).
