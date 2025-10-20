-module(lib_misc).
-include_lib("kernel/include/file.hrl").
-export([
         deliberate_error/1,
         deliberate_error1/1,
         random_seed/0,
         string2value/1,
         ls/1,
         file_size_and_type/1,
         unconsult/2,
         dump/2,
         consult/1,
         for/3,
         qsort/1,
         pythag/1,
         perms/1,
         max/2,
         filter/2,
         odds_and_evens/1,
         odds_and_evens2/1,
         count_characters/1,
         sqrt/1,
         sum/1,
         sleep/1,
         flush_buffer/0,
         priority_receive/0
        ]).

for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].

qsort([]) -> [];
qsort([Pivot|T]) ->
    qsort([X || X <- T, X < Pivot])
    ++ [Pivot] ++
    qsort([X || X <- T, X >= Pivot]).


pythag(N) ->
    [ {A, B, C} ||
      A <- lists:seq(1, N),
      B <- lists:seq(1, N),
      C <- lists:seq(1, N),
      A+B+C =< N,
      A*A+B*B =:= C*C
    ].


perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L -- [H])].


max(X, Y) when X > Y -> X;
max(X, Y) -> Y.


filter(P, [H|T]) ->
    case P(H) of
        true  -> [H|filter(P, T)];
        false -> filter(P, T)
    end;
filter(P, []) -> [].


odds_and_evens(L) ->
    Odds  = [X || X <- L, (X rem 2) =:= 1],
    Evens = [X || X <- L, (X rem 2) =:= 0],
    {Odds, Evens}.


odds_and_evens2(L) ->
    odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H|T], Odds, Evens) ->
    case (H rem 2) of
        1 -> odds_and_evens_acc(T, [H|Odds], Evens);
        0 -> odds_and_evens_acc(T, Odds, [H|Evens])
    end;
odds_and_evens_acc([], Odds, Evens) ->
    {lists:reverse(Odds), lists:reverse(Evens)}.


count_characters(Str) ->
    count_characters(Str, #{}).

count_characters([H|T], X) ->
    case maps:is_key(H, X) of
        false -> count_characters(T, X#{ H => 1 });
        true  -> #{ H := N } = X,
                 count_characters(T, X#{ H := N+1 })
    end;
count_characters([], X) ->
    X.


sqrt(X) when X < 0 ->
    error({squareRootNegativeArgument, X});
sqrt(X) ->
    math:sqrt(X).


sum(L) -> sum(L, 0).

sum([], N)	    -> N;
sum([H|T], N)	-> sum(T, H+N).


sleep(T) ->
    receive
    after T -> true
    end.


flush_buffer() ->
    receive
        _Any -> flush_buffer()
    after 0 -> true
    end.

priority_receive() ->
    receive
        {alarm, X} -> {alarm, X}
    after 0 ->
              receive
                  Any -> Any
              end
    end.

consult(File) ->
    case file:open(File, read) of
        {ok, S} ->
            Val = consult1(S),
            file:close(S),
            {ok, Val};
        {error, Why} ->
            {error, Why}
    end.

consult1(S) ->
    case io:read(S) of
        {ok, Term} -> [Term | consult1(S)];
        eof        -> [];
        Error      -> Error
    end.

dump(File, Term) ->
    Out = File ++ ".tmp",
    io:format("** dumping to ~s~n", [Out]),
    {ok, S} = file:open(Out, [write]),
    io:format(S, "~p.~n", [Term]),
    file:close(S).

unconsult(File, L) ->
    {ok, S} = file:open(File, write),
    lists:foreach(fun(X) -> io:format(S, "~p~n", [X]) end, L),
    file:close(S).

file_size_and_type(File) ->
    case file:read_file_info(File) of
        {ok, Facts} ->
            {Facts#file_info.type, Facts#file_info.size};
        _ ->
            error
    end.

ls(Dir) ->
    {ok, L} = file:list_dir(Dir),
    lists:map(fun(I) -> {I, file_size_and_type(I)} end, lists:sort(L)).

string2value(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),
    Bindings = erl_eval:new_bindings(),
    {value, Value, _} = erl_eval:exprs(Exprs, Bindings),
    Value.

random_seed() ->
    {_,_,X} = erlang:timestamp(),
    {H,M,S} = time(),
    H1 = H * X rem 32767,
    M1 = M * X rem 32767,
    S1 = S * X rem 32767,
    put(random_seed, {H1,M1,S1}).


deliberate_error(A) ->
    bad_function(A, 12),
    lists:reverse(A).

bad_function(A, _) ->
    {ok, Bin} = file:open({abc, 123}, A),
    binary_to_list(Bin).

deliberate_error1(A) ->
    bad_function(A, 12).
