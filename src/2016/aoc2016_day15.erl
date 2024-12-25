-module(aoc2016_day15).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day15_ex.txt", star1, 5},
        {"examples/2016/day15_ex.txt", star2, 85}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 15},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Discs) ->
    Reminders = [find_reminder(D) || D <- Discs],
    io:format("~p~n", [Reminders]),
    {Res, _} = tools:chinese_remainder(Reminders),
    Res.

star2(Discs) ->
    Reminders = [find_reminder(D) || D <- [[length(Discs) + 1, 11, 0] | Discs]],
    io:format("~p~n", [Reminders]),
    {Res, _} = tools:chinese_remainder(Reminders),
    Res.

read(File) ->
    tools:read_lines(File, fun parse_disc/1).

parse_disc(Line) ->
    tools:parse_format(Line, "Disc #~d has ~d positions; at time=0, it is at position ~d.").

find_reminder([Disc, Positions, At]) ->
    M = Positions,
    R = tools:mod(Positions - Disc - At, M),
    {R, M}.
