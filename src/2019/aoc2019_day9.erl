-module(aoc2019_day9).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 9},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Program) ->
    Result = intcode:run(Program, [{input, [1]}]),
    hd(intcode:get_output(Result)).

star2(Program) ->
    Result = intcode:run(Program, [{input, [2]}]),
    hd(intcode:get_output(Result)).

read(File) ->
    intcode:from_file(File).
