-module({'my_stateful_module', State}).
-export([init/1, get_state/0, update_state/1]).

init(InitialState) ->
    {'my_stateful_module', InitialState}.

get_state() ->
    element(2, ?MODULE). % Access the state from the module's name

update_state(NewState) ->
    % This is where the limitation lies. A module's name cannot be changed at runtime.
    % To truly update the state, you would typically need to load a new module
    % with the updated state in its name, which is impractical for dynamic state changes.
    % This is why gen_server is the preferred approach for stateful processes.
    io:format("Cannot directly update state in a tuple module name.~n").
