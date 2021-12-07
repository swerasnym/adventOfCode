-module(aoc2021_day7).

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
    minimize(Data, fun value/2).

star2(Data) ->
    minimize(Data, fun value2/2).

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
