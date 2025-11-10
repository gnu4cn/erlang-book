-module(ptests).
-export([tests/1]).
-import(lists, [map/2]).

tests([N]) ->
    Nsched = list_to_integer(atom_to_list(N)),
    run_tests(1, Nsched).

run_tests(N, Nsched) ->
    case test(N) of
        stop ->
            init:stop();
        Val ->
            io:format("~p.~n", [{Nsched, Val}]),
            run_tests(N+1, Nsched)
    end.

test(1) ->
    %% Make 100 lists
    %%   Each list contains 1000 random integers
    seed(),
    S = lists:seq(1,100),
    L = map(fun(_) -> mkList(1000) end, S),

    {Time1, S1} = timer:tc(lists,    map,  [fun lists:sort/1, L], microsecond),
    {Time2, S2} = timer:tc(lib_misc, pmap, [fun lists:sort/1, L], microsecond),
    {sort, Time1, Time2, equal(S1, S2)};
test(2) ->
    %% L = [27,27,27,..] 100 times
    L = lists:duplicate(100, 27),

    {Time1, S1} = timer:tc(lists,    map,  [fun lib_misc:fib/1, L], microsecond),
    {Time2, S2} = timer:tc(lib_misc, pmap, [fun lib_misc:fib/1, L], microsecond),
    {fib, Time1, Time2, equal(S1, S2)};
test(3) ->
    stop.

equal(S, S)  -> true;
equal(S1,S2) -> {differ, S1, S2}.

seed() -> rand:seed(exsss, {44,45,46}).

mkList(K) -> mkList(K, []).

mkList(0, L) -> L;
mkList(N, L) -> mkList(N-1, [rand:uniform(1000*1000)|L]).
