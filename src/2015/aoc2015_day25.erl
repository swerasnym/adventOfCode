-module(aoc2015_day25).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, {3, 3}}, star1, 1601130},
        {{data, {5, 4}}, star1, 6899651},
        {{data, {3, 6}}, star1, 16474243}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 25},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({R, C}) ->
    D = R + C - 1,
    Idx = (D * (D - 1) div 2) + C,
    tools:repeat(Idx - 1, fun generate/1, 20151125).

star2(_Data) ->
    {done, "Start the weather machine!"}.

read(File) ->
    [[R, C]] = tools:read_multiple_formats(
        File,
        "To continue, please consult the code grid in the manual.  Enter the code at row ~d, column ~d."
    ),
    {R, C}.

generate(Prev) ->
    (Prev * 252533) rem 33554393.
