-module(aoc2023_day14).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2023, 14}}).

run() ->
    %   aoc_solution:run(?MODULE, all, "2023/data/day14_ex.txt").
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Grid) ->
    Grid1 = tilt_north(Grid, 0, 0),
    print(Grid1),
    load(Grid1).

star2(Grid) ->
    erlang:erase(),
    End = cycle([], Grid, 1000000001),
    erlang:erase(),
    load(End).

read(File) ->
    tools:read_grid(File, #{$O => round, $# => cube, $. => empty}).

cycle(_, Grid, 0) ->
    io:nl(),
    Grid;
cycle([], Grid, N) ->
    io:format("."),
    cycle([north, west, south, east], Grid, N - 1);
cycle([H | Rest], Grid, N) ->
    Key = {H, Grid},
    Res = tilt(H, Grid),
    case erlang:get(Key) of
        undefined ->
            erlang:put(Key, N),
            cycle(Rest, Res, N);
        Fn when Fn - N < N ->
            Length = Fn - N,
            Cycles = N div Length,
            io:format("~nLoop: Fn:~p N:~p L:~p C:~p ~n", [Fn, N, Length, Cycles]),
            cycle(Rest, Res, N - Length * Cycles);
        _ ->
            cycle(Rest, Res, N)
    end.

tilt(north, Grid) ->
    tilt_north(Grid, 0, 0);
tilt(east, Grid) ->
    tools:rotate_grid(tilt_north(tools:rotate_grid(Grid, ccw), 0, 0), cw);
tilt(west, Grid) ->
    tools:rotate_grid(tilt_north(tools:rotate_grid(Grid, cw), 0, 0), ccw);
tilt(south, Grid) ->
    tools:flip_grid(tilt_north(tools:flip_grid(Grid, y), 0, 0), y).

tilt_north(#{max := {_, Y}} = G, _, Y) ->
    G;
tilt_north(#{max := {Xmax, _}} = G, X, Y) when X > Xmax ->
    tilt_north(G, 0, Y + 1);
tilt_north(G, X, Y) ->
    G1 =
        case {maps:get({X, Y}, G), next({X, Y + 1}, {0, 1}, G)} of
            {empty, {round, Pos}} ->
                G#{{X, Y} := round, Pos := empty};
            _ ->
                G
        end,
    tilt_north(G1, X + 1, Y).

next({X, Y}, _, #{max := {Xmax, Ymax}}) when X > Xmax orelse Y > Ymax ->
    empty;
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
