-module(aoc2021_day7).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 7},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    {Res, _} = minimize(Data, fun value/2),
    Res.

star2(Data) ->
    {Res, _} = minimize(Data, fun value2/2),
    Res.

read(File) ->
    tools:read_integers(File, ",").

minimize(Data, F) ->
    Min = lists:min(Data),
    Max = lists:max(Data),
    Values = [{F(Data, Pos), Pos} || Pos <- lists:seq(Min, Max)],
    lists:min(Values).

value(Data, Pos) ->
    lists:sum([abs(D - Pos) || D <- Data]).

value2(Data, Pos) ->
    lists:sum([abs(D - Pos) * (abs(D - Pos) + 1) div 2 || D <- Data]).
