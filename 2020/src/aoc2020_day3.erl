-module(aoc2020_day3).

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
    toboggan(Grid, {3, 1}).

star2(Grid) ->
    Dirs = [{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}],
    tools:product([toboggan(Grid, Dir) || Dir <- Dirs]).

read(File) ->
    tools:read_grid(File, #{$# => tree, $. => open}).

toboggan(#{max := {_Xmax, Ymax}}, {Dx, Dy} = Grid) ->
    tools:count(tree, [get_pos(N * Dx, N * Dy, Grid) || N <- lists:seq(0, Ymax div Dy)]).

get_pos(X, Y, #{max := {Xmax, _Ymax}} = Grid) ->
    maps:get({X rem (Xmax + 1), Y}, Grid).
