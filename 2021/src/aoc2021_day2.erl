-module(aoc2021_day2).

-export([run/2]).

run(Star, File) ->
    Data = read(File),
    case Star of
        star1 ->
            star1(Data);
        star2 ->
            star2(Data);
        _ ->
            Star1 = star1(Data),
            Star2 = star2(Data),
            {Star1, Star2}
    end.

star1(Data) ->
    move(Data, 0, 0).

star2(Data) ->
    move_aim(Data, 0, 0, 0).

read(File) ->
    tools:read_format(File, "~a ~d").

move([[forward, F] | Rest], Horisontal, Depth) ->
    move(Rest, Horisontal + F, Depth);
move([[down, D] | Rest], Horisontal, Depth) ->
    move(Rest, Horisontal, Depth + D);
move([[up, U] | Rest], Horisontal, Depth) ->
    move(Rest, Horisontal, Depth - U);
move([], Horisontal, Depth) ->
    Horisontal * Depth.

move_aim([[forward, F] | Rest], Horisontal, Depth, Aim) ->
    move_aim(Rest, Horisontal + F, Depth + Aim * F, Aim);
move_aim([[down, D] | Rest], Horisontal, Depth, Aim) ->
    move_aim(Rest, Horisontal, Depth, Aim + D);
move_aim([[up, U] | Rest], Horisontal, Depth, Aim) ->
    move_aim(Rest, Horisontal, Depth, Aim - U);
move_aim([], Horisontal, Depth, _Aim) ->
    Horisontal * Depth.
