-module(aoc2021_day2).
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
        problem => {2021, 2},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    move(Data, 0, 0).

star2(Data) ->
    move_aim(Data, 0, 0, 0).

read(File) ->
    tools:read_multiple_formats(File, "~a ~d").

move([[forward, F] | Rest], Horizontal, Depth) ->
    move(Rest, Horizontal + F, Depth);
move([[down, D] | Rest], Horizontal, Depth) ->
    move(Rest, Horizontal, Depth + D);
move([[up, U] | Rest], Horizontal, Depth) ->
    move(Rest, Horizontal, Depth - U);
move([], Horizontal, Depth) ->
    Horizontal * Depth.

move_aim([[forward, F] | Rest], Horizontal, Depth, Aim) ->
    move_aim(Rest, Horizontal + F, Depth + Aim * F, Aim);
move_aim([[down, D] | Rest], Horizontal, Depth, Aim) ->
    move_aim(Rest, Horizontal, Depth, Aim + D);
move_aim([[up, U] | Rest], Horizontal, Depth, Aim) ->
    move_aim(Rest, Horizontal, Depth, Aim - U);
move_aim([], Horizontal, Depth, _Aim) ->
    Horizontal * Depth.
