-module(aoc2017_day5).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day5_ex.txt", star1, 5},
        {"examples/2017/day5_ex.txt", star2, 10}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 5},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    jump(1, Map, 0).

star2(Map) ->
    jump2(1, Map, 0).

read(File) ->
    maps:from_list(lists:enumerate(tools:read_integers(File))).

jump(Pos, Map, Steps) ->
    case maps:get(Pos, Map, outside) of
        outside ->
            Steps;
        Jump ->
            jump(Pos + Jump, Map#{Pos => Jump + 1}, Steps + 1)
    end.

jump2(Pos, Map, Steps) ->
    case maps:get(Pos, Map, outside) of
        outside ->
            Steps;
        Jump when Jump >= 3 ->
            jump2(Pos + Jump, Map#{Pos => Jump - 1}, Steps + 1);
        Jump ->
            jump2(Pos + Jump, Map#{Pos => Jump + 1}, Steps + 1)
    end.
