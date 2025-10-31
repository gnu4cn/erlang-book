-module(my_stateful_server).
-behaviour(gen_server).

-export([start_link/0, get_state/0, increment/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_state() ->
    gen_server:call(?MODULE, get_state).

increment() ->
    gen_server:cast(?MODULE, increment).

init([]) ->
    {ok, 0}. % Initial state is 0

handle_call(get_state, _From, State) ->
    {reply, State, State}.

handle_cast(increment, State) ->
    NewState = State + 1,
    {noreply, NewState}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
