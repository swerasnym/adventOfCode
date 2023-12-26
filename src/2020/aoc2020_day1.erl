-module(aoc2020_day1).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2020/data/dayN_ex.txt", star1, unknown},
        % {"2020/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2020, 1},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).
star1(Data) ->
    hd([
        Value1 * Value2
     || Value1 <- Data, Value2 <- Data, Value1 + Value2 == 2020, Value1 =< Value2
    ]).

star2(Data) ->
    hd([
        Value1 * Value2 * Value3
     || Value1 <- Data,
        Value2 <- Data,
        Value3 <- Data,
        Value1 + Value2 + Value3 == 2020,
        Value1 =< Value2,
        Value2 =< Value3
    ]).

read(File) ->
    tools:read_integers(File).
