-module(aoc2023_day25).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"2023/data/day25_ex.txt", star1, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 25},
        examples => Examples,
        all => [star1]
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    io:format("~120p", [Data]),
    unknown.

star2(Data) ->
    Data,
    unknown.

read(File) ->
    tools:read_lines(File).
