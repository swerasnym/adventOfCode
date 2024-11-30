-module(aoc2015_day15).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2015/day15_ex.txt", star1, 62842880},
        {"examples/2015/day15_ex.txt", star2, 57600000}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 15},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Properties = [lists:droplast(P) || P <- Data],
    Combinations = combine(Properties, 100),
    lists:max([sum_positive(C) || C <- Combinations]).

star2(Data) ->
    Combinations = combine(Data, 100),
    With500Cal = [lists:droplast(C) || C <- Combinations, lists:last(C) == 500],
    lists:max([sum_positive(C) || C <- With500Cal]).

read(File) ->
    tools:read_multiple_formats(
        File, "~*s capacity ~d, durability ~d, flavor ~d, texture ~d, calories ~d"
    ).

combine([F], N) ->
    [aoc_vector:mul(F, N)];
combine([F | Rest], N) ->
    [aoc_vector:add(aoc_vector:mul(K, F), V) || K <- lists:seq(0, N), V <- combine(Rest, N - K)].

sum_positive(List) ->
    tools:product([L || L <- List, L > 0]).
