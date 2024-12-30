-module(aoc2016_day12).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day12_ex.txt", star1, 42},
        {"examples/2016/day12_ex.txt", star2, 42}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 12},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Bun) ->
    {halt, End} = aoc_bun:run(Bun),
    #{a := A} = aoc_bun:get_mem(End),
    A.

star2(Bun0) ->
    Bun1 = aoc_bun:set_mem(Bun0, #{c => 1}),
    Bun = aoc_bun:set_opt(jint, true, Bun1),
    {halt, End} = aoc_bun:run(Bun),
    #{a := A} = aoc_bun:get_mem(End),
    A.

read(File) ->
    aoc_bun:from_file(File).
