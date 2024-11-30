-module(aoc2015_day20).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 20}
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Goal) ->
    Max = Goal div 20,
    Ref = atomics:new(Max, []),
    atomics:put(Ref, 1, 1),
    Res = sigma_generator(Ref, 2, Max, Goal),
    LowValues = [{N, atomics:get(Ref, N)} || N <- lists:seq(2, 100)],
    io:format("~p~n", [LowValues]),

    Res.

star2(Goal) ->
    Max = Goal div 11,
    Ref = atomics:new(Max, []),
    atomics:put(Ref, 1, 11),
    Res = add_until(Ref, 1, Max, Goal, 50),
    LowValues = [{N, atomics:get(Ref, N)} || N <- lists:seq(2, 100)],
    io:format("~p~n", [LowValues]),
    Res.

read(File) ->
    [N] = tools:read_integers(File),
    N.

add_until(_Ref, Max, Max, _Goal, _Additions) ->
    error;
add_until(Ref, N, Max, Goal, Additions) ->
    Value = atomics:add_get(Ref, N, N * 11),
    case Value >= Goal of
        true ->
            N;
        false ->
            add(Ref, N + N, N, Max, Additions - 1),
            add_until(Ref, N + 1, Max, Goal, Additions)
    end.

add(_Ref, _N, _Inc, _Max, 0) ->
    ok;
add(_Ref, N, _Inc, Max, _) when N > Max ->
    ok;
add(Ref, N, Inc, Max, Additions) ->
    atomics:add(Ref, N, Inc * 11),
    add(Ref, N + Inc, Inc, Max, Additions - 1).

sigma_generator(_Ref, Max, Max, _Goal) ->
    error;
sigma_generator(Ref, N, Max, Goal) ->
    case atomics:get(Ref, N) of
        0 ->
            %Prime: σ(N) = N+1
            Sigma = N + 1,
            atomics:put(Ref, N, Sigma),
            mark_composite(Ref, N * N, N, Max);
        P ->
            {PN, Sum} = pp(N, P),
            Sigma = Sum * atomics:get(Ref, N div PN),
            atomics:put(Ref, N, Sigma)
    end,
    case (Sigma * 10) >= Goal of
        true ->
            N;
        false ->
            sigma_generator(Ref, N + 1, Max, Goal)
    end.

mark_composite(_Ref, N, _P, Max) when N >= Max ->
    ok;
mark_composite(Ref, N, P, Max) ->
    atomics:put(Ref, N, P),
    mark_composite(Ref, N + P, P, Max).

pp(N, P) ->
    pp(N, P, P, 1).

pp(N, P, PN, Sum) ->
    case N rem PN of
        0 ->
            pp(N, P, PN * P, Sum + PN);
        _ ->
            {PN div P, Sum}
    end.
