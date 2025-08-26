clear_status(#todo{status=S, who=W} = R) ->
    %% Inside this function S and W are bound to the field
    %% values in the record
    %%
    %% R is the *entire* record
    R#todo{status=finished}
    %% ...


do_something(X) when is_record(X, todo) ->
    %% ...
