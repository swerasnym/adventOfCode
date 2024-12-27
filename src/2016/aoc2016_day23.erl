-module(aoc2016_day23).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day23_ex.txt", star1, 3}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 23},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Bun0) ->
    Bun1 = aoc_bun:set_mem(Bun0, #{a => 7}),
    Bun = aoc_bun:set_opt(jint, true, Bun1),
    End = aoc_bun:run(Bun),
    #{a := A} = aoc_bun:get_mem(End),
    A.

star2(Bun0) ->
    Bun1 = aoc_bun:set_mem(Bun0, #{a => 12}),
    Bun = aoc_bun:set_opt(jint, true, Bun1),
    End = aoc_bun:run(Bun),
    #{a := A} = aoc_bun:get_mem(End),
    A.

read(File) ->
    aoc_bun:from_file(File).
