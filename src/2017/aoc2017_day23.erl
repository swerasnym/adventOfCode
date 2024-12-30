-module(aoc2017_day23).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 23}
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Lines) ->
    [io:format("~s~n", [L]) || L <- Lines],
    Program = aoc_tablet:from_lines(Lines),
    {halt, S} = aoc_tablet:run(aoc_tablet:debug(Program, true)),
    Calls = aoc_tablet:get_calls(S),
    maps:get(mul, Calls).

star2(Lines) ->
    Patched = patch(Lines),
    [io:format("~s~n", [L]) || L <- Patched],
    Program = aoc_tablet:from_lines(Patched),
    {halt, S} = aoc_tablet:run(aoc_tablet:set_mem(Program, #{a => 1})),
    Mem = aoc_tablet:get_mem(S),
    maps:get(h, Mem).

read(File) ->
    tools:read_lines(File).

patch(Lines) ->
    Map = maps:from_list(lists:enumerate(Lines)),
    "set " ++ [X1 | _] = maps:get(1, Map),
    "mul " ++ [X13, $\s, Y13] = maps:get(13, Map),
    "sub " ++ [X13, $\s, X1] = maps:get(14, Map),
    Patch = #{
        13 => "set " ++ [X13, $\s, X1],
        14 => "mod " ++ [X13, $\s, Y13],
        16 => "jnz 1 10",
        24 => "jnz 0 0"
    },
    Merged = maps:merge(Map, Patch),
    [L || _ := L <- maps:iterator(Merged, ordered)].
