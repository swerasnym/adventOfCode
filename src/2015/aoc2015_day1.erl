-module(aoc2015_day1).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2015/day1_ex.txt", star1, 3},
        {"examples/2015/day1_ex.txt", star2, 1}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 1},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Counts = tools:count(Data),
    maps:get($(, Counts) - maps:get($), Counts).

star2(Data) ->
    basement(0, 0, Data).

read(File) ->
    tools:read_string(File).

basement(-1, Steps, _) ->
    Steps;
basement(Level, Steps, [$( | Rest]) ->
    basement(Level + 1, Steps + 1, Rest);
basement(Level, Steps, [$) | Rest]) ->
    basement(Level - 1, Steps + 1, Rest).
