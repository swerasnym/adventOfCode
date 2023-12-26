-module(aoc2019_day21).
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
        problem => {2019, 21},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Program) ->
    Code = "OR A J\nAND B J\nAND C J\nNOT J J\nAND D J\nWALK\n",
    Result = intcode:run(Program, [{input, Code}]),
    Output = intcode:get_output(Result),

    io:format("~s~s", [Code, lists:droplast(Output)]),
    lists:last(Output).

star2(Program) ->
    Code =
        "OR A T\nAND B T\nAND C T\nNOT T T\nOR E J\nOR H J\nAND D J\nAND "
        "T J\nRUN\n",
    Result = intcode:run(Program, [{input, Code}]),
    Output = intcode:get_output(Result),

    io:format("~s~s", [Code, lists:droplast(Output)]),
    lists:last(Output).

read(File) ->
    intcode:from_file(File).
