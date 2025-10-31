-module(my_state_machine).
-behaviour(gen_statem).

-export([start_link/0, trigger_event/1]).
-export([init/1, callback_mode/0, locked/3, open/3]).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

trigger_event(Event) ->
    gen_statem:cast(?MODULE, Event).

init([]) ->
    process_flag(trap_exit, true),
    {ok, locked, #{}}. % Initial state: locked, empty data

callback_mode() -> state_functions.

locked(cast, {button, Digit}, Data) ->
    case Digit of
        1 -> {next_state, open, Data};
        _ -> {keep_state, Data}
    end;
locked(_EventType, _EventContent, Data) ->
    {keep_state, Data}.

open(cast, close_button, Data) ->
    {next_state, locked, Data};
open(_EventType, _EventContent, Data) ->
    {keep_state, Data}.
