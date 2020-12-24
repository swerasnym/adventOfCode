-module(day3).

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

star1(Grid) ->
    {_Xmax, Ymax} = maps:get(max, Grid),
    Results = [get_pos(Y * 3, Y, Grid) || Y <- lists:seq(0, Ymax)],
    tools:count(tree, Results).

star2(Grid) ->
    {_Xmax, Ymax} = maps:get(max, Grid),
    T1 = tools:count(tree, [get_pos(Y * 1, Y, Grid) || Y <- lists:seq(0, Ymax)]),
    T2 = tools:count(tree, [get_pos(Y * 3, Y, Grid) || Y <- lists:seq(0, Ymax)]),
    T3 = tools:count(tree, [get_pos(Y * 5, Y, Grid) || Y <- lists:seq(0, Ymax)]),
    T4 = tools:count(tree, [get_pos(Y * 7, Y, Grid) || Y <- lists:seq(0, Ymax)]),
    T5 = tools:count(tree, [get_pos(Y, Y * 2, Grid) || Y <- lists:seq(0, Ymax div 2)]),
    T1 * T2 * T3 * T4 * T5.

read(File) ->
    tools:read_grid(File, #{$# => tree, $. => open}).

get_pos(X, Y, #{max := {Xmax, _Ymax}} = Grid) ->
    maps:get({X rem (Xmax + 1), Y}, Grid).
