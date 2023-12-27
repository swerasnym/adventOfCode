-module(aoc_example).
-behaviour(aoc_solution).

-export([info/0, star1/1, star2/1, star2/2, read/1]).

info() ->
    Data = [1, 2, 3, 4, 5, 6],

    Examples = [
        {{data, Data}, star1, 21},
        {"examples/example.txt", star1, 21},
        {"examples/example.txt", {star2, 2}, 3},
        {"examples/example.txt", {star2, 3}, 6}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {0, 0},
        examples => Examples
    }).

star1(Values) ->
    lists:sum(Values).

star2(Values) ->
    star2(Values, 17).

star2(Values, P) ->
    lists:sum([V rem P || V <- Values]).

read(File) ->
    tools:read_integers(File).
