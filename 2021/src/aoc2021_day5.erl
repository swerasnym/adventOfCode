-module(aoc2021_day5).

-export([run/2, points/1]).

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
    HorV = lists:filter(fun v_or_h/1, Data),
    AllPoints = lists:flatmap(fun(List) -> points(List) end, HorV),
    Count = tools:count(AllPoints),
    Intersections = maps:filter(fun(_, V) -> V > 1 end, Count),
    maps:size(Intersections).

star2(Data) ->
    AllPoints = lists:flatmap(fun(List) -> points(List) end, Data),
    Count = tools:count(AllPoints),
    Intersections = maps:filter(fun(_, V) -> V > 1 end, Count),
    maps:size(Intersections).

read(File) ->
    tools:read_format(File, "~d,~d -> ~d,~d").

v_or_h([X, _, X, _]) ->
    true;
v_or_h([_, Y, _, Y]) ->
    true;
v_or_h(_) ->
    false.

points([X, Y1, X, Y2]) when Y1 =< Y2 ->
    [{X, Y} || Y <- lists:seq(Y1, Y2)];
points([X, Y2, X, Y1]) ->
    [{X, Y} || Y <- lists:seq(Y1, Y2)];
points([X1, Y, X2, Y]) when X1 =< X2 ->
    [{X, Y} || X <- lists:seq(X1, X2)];
points([X2, Y, X1, Y]) ->
    [{X, Y} || X <- lists:seq(X1, X2)];
points([X1, Y1, X2, Y2]) when X1 =< X2, Y1 =< Y2 ->
    [{X1 + D, Y1 + D} || D <- lists:seq(0, X2 - X1)];
points([X1, Y1, X2, _Y2]) when X1 =< X2 ->
    [{X1 + D, Y1 - D} || D <- lists:seq(0, X2 - X1)];
points([X1, Y1, X2, Y2]) when Y1 =< Y2 ->
    [{X1 - D, Y1 + D} || D <- lists:seq(0, X1 - X2)];
points([X1, Y1, X2, _Y2]) ->
    [{X1 - D, Y1 - D} || D <- lists:seq(0, X1 - X2)].
