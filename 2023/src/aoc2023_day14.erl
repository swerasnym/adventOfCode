-module(aoc2023_day14).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2023, 14}}).

run() ->
    % aoc_solution:run(?MODULE, all, "2023/data/day14_ex.txt").
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Grid) ->
    Grid1 = tilt(Grid, north),
    print(Grid1),
    load(Grid1).

star2(Grid) ->
    erlang:erase(),
    End = cycle(Grid, 1000000000),
    erlang:erase(),
    load(End).

read(File) ->
    tools:read_grid(File, #{$O => round, $# => cube, $. => empty}).

cycle(Grid, 0) ->
    Grid;
cycle(Grid, N) ->
    case erlang:get(Grid) of
        undefined ->
            North = tilt(Grid, north),
            West = tilt(North, west),
            South = tilt(West, south),
            East = tilt(South, east),
            erlang:put(Grid, {N, East}),
            cycle(East, N - 1);
        {Fn, Res} when Fn - N < N ->
            Length = Fn - N,
            Cycles = (N - 1) div Length,
            cycle(Res, N - 1 - Length * Cycles);
        {_, Res} ->
            cycle(Res, N - 1)
    end.

tilt(#{max := {Xmax, Ymax}} = G, north) ->
    tilt(G, {0, 1}, [{X, Y} || Y <- lists:seq(0, Ymax - 1), X <- lists:seq(0, Xmax)]);
tilt(#{max := {Xmax, Ymax}} = G, west) ->
    tilt(G, {1, 0}, [{X, Y} || X <- lists:seq(0, Xmax - 1), Y <- lists:seq(0, Ymax)]);
tilt(#{max := {Xmax, Ymax}} = G, south) ->
    tilt(G, {0, -1}, [{X, Y} || Y <- lists:seq(Ymax, 1, -1), X <- lists:seq(0, Xmax)]);
tilt(#{max := {Xmax, Ymax}} = G, east) ->
    tilt(G, {-1, 0}, [{X, Y} || X <- lists:seq(Xmax, 1, -1), Y <- lists:seq(0, Ymax)]).

tilt(G, DxDy, PosList) ->
    lists:foldl(fun({X, Y}, Acc) -> tilt(Acc, X, Y, DxDy) end, G, PosList).

tilt(G, X, Y, {Dx, Dy}) ->
    case maps:get({X, Y}, G) of
        empty ->
            case next({X + Dx, Y + Dy}, {Dx, Dy}, G) of
                {round, Pos} ->
                    G#{{X, Y} := round, Pos := empty};
                _ ->
                    G
            end;
        _ ->
            G
    end.

next({X, _} = Pos, {_, 0}, #{max := {X, _}} = G) ->
    {maps:get(Pos, G), Pos};
next({_, Y} = Pos, {0, _}, #{max := {_, Y}} = G) ->
    {maps:get(Pos, G), Pos};
next({0, _} = Pos, {_, 0}, G) ->
    {maps:get(Pos, G), Pos};
next({_, 0} = Pos, {0, _}, G) ->
    {maps:get(Pos, G), Pos};
next({X, Y} = Pos, {Dx, Dy}, G) ->
    case maps:get(Pos, G) of
        empty ->
            next({X + Dx, Y + Dy}, {Dx, Dy}, G);
        Type ->
            {Type, Pos}
    end.

load(#{max := {_, Ymax}} = G) ->
    lists:sum([1 + Ymax - Yk || {_, Yk} := round <- G]).

print(Grid) ->
    F = fun
        (round) -> $O;
        (cube) -> $#;
        (empty) -> $.;
        (M) -> M
    end,

    tools:print_grid(
        tools:replace(Grid, F)
    ),
    io:nl().
