-module(aoc2015_day2).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2015/day2_ex.txt", star1, 101},
        {"examples/2015/day2_ex.txt", star2, 48}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 2},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Presents) ->
    lists:sum([paper(P) || P <- Presents]).

star2(Presents) ->
    lists:sum([ribbon(P) || P <- Presents]).

read(File) ->
    tools:read_multiple_formats(File, "~dx~dx~d").

paper(Dims) ->
    [A, B, C] = lists:sort(Dims),
    3 * A * B + 2 * A * C + 2 * B * C.

ribbon(Dims) ->
    [A, B, C] = lists:sort(Dims),
    2 * (A + B) + A * B * C.
