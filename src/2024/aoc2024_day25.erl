-module(aoc2024_day25).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day25_ex.txt", star1, 3}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 25},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(LockAndKey) ->
    {Locks, Keys} = lists:partition(fun(M) -> maps:get({0, 0}, M) == $# end, LockAndKey),
    length([{L, K} || L <- Locks, K <- Keys, no_overlaps(L, K)]).

star2(_) ->
    {done, "Deliver The Chronicle!"}.

read(File) ->
    tools:read_blocks(File, fun tools:parse_grid/1).

no_overlaps(L1, L2) ->
    K1 = lists:sort([K || K := $# <- L1]),
    K2 = lists:sort([K || K := $# <- L2]),
    tools:overlap(K1, K2) == [].
