-module(aoc2016_day19).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, 5}, star1, 3},
        {{data, 13}, star1, 11},
        {{data, 21}, star1, 11},
        {{data, 42}, star1, 21},
        {{data, 78}, star1, 29}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 19},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(N) ->
    {Res, _} = josephus(N, 2, #{}),
    Res + 1.

star2(N) ->
    solve(1, N).

read(File) ->
    erlang:list_to_integer(tools:read_string(File)).

josephus(1, _, Mem) ->
    {0, Mem};
josephus(N, 1, Mem) ->
    {N - 1, Mem};
josephus(N, K, Mem) when K > N ->
    Key = {N, K},
    case maps:get(Key, Mem, first) of
        first ->
            {Res0, Mem1} = josephus(N - 1, K, Mem),
            Res = tools:mod(Res0 + K, N),
            {Res, Mem1#{Key => Res}};
        V ->
            {V, Mem}
    end;
josephus(N, K, Mem) ->
    Key = {N, K},
    case maps:get(Key, Mem, first) of
        first ->
            Cnt = (N div K),
            {Res0, Mem1} = josephus(N - Cnt, K, Mem),
            Res1 = Res0 - tools:mod(N, K),
            case Res1 < 0 of
                true ->
                    Res = Res1 + N,
                    {Res, Mem1#{Key => Res}};
                false ->
                    Res = Res1 + (Res1 div (K - 1)),
                    {Res, Mem1#{Key => Res}}
            end;
        V ->
            {V, Mem}
    end.

solve(I, Target) when I * 3 < Target ->
    solve(I * 3, Target);
solve(I, Target) ->
    Target - I.
