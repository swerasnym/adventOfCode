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
    tools:read_format(File, "~s ~d").

move([["forward", F] | Rest], X, Y) ->
    move(Rest, X + F, Y);
move([["down", D] | Rest], X, Y) ->
    move(Rest, X, Y + D);
move([["up", U] | Rest], X, Y) ->
    move(Rest, X, Y - U);
move([], X, Y) ->
    X * Y.

move_aim([["forward", F] | Rest], X, Y, A) ->
    move_aim(Rest, X + F, Y + A * F, A);
move_aim([["down", D] | Rest], X, Y, A) ->
    move_aim(Rest, X, Y, A + D);
move_aim([["up", U] | Rest], X, Y, A) ->
    move_aim(Rest, X, Y, A - U);
move_aim([], X, Y, _A) ->
    X * Y.
