-module(aoc2023_day21).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, star2/2, read/1]).

info() ->
    Examples = [
        %% From problem description
        {"examples/2023/day21_ex.txt", {star1, 6}, 16},
        {"examples/2023/day21_ex.txt", {star1, 10}, 50},
        {"examples/2023/day21_ex.txt", {star1, 100}, 6536},
        {"examples/2023/day21_ex.txt", {star1, 500}, 167004},
        %% Mofidied to have a path trought the center
        {"examples/2023/day21_ex2.txt", {star1, 6}, 36},
        {"examples/2023/day21_ex2.txt", {star1, 6 + 11}, 239},
        {"examples/2023/day21_ex2.txt", {star1, 6 + 22}, 624},
        {"examples/2023/day21_ex2.txt", {star1, 6 + 55}, 2871},
        {"examples/2023/day21_ex2.txt", {star1, 6 + 99}, 8415},
        {"examples/2023/day21_ex2.txt", {star2, 6}, 36},
        {"examples/2023/day21_ex2.txt", {star2, 6 + 11}, 239},
        {"examples/2023/day21_ex2.txt", {star2, 6 + 22}, 624},
        {"examples/2023/day21_ex2.txt", {star2, 6 + 55}, 2871},
        {"examples/2023/day21_ex2.txt", {star2, 6 + 99}, 8415},
        {"examples/2023/day21_ex2.txt", {star1, 6 + 100}, 8589},
        {"examples/2023/day21_ex2.txt", {star2, 6 + 100}, 8589},
        {"examples/2023/day21_ex2.txt", {star2, 6 + 1100}, 921236}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 21},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).
star1(Data) ->
    star1(Data, 64).
star1({Start, Grid}, Steps) ->
    Visited = bfs2([{Start, 0}], Grid, #{}, Steps),
    count(Visited, Steps).
star2(Data) ->
    star2(Data, 26501365).
star2({Start, #{max := {Xmax, _Ymax}} = Grid}, Steps) ->
    X = (Xmax + 1),
    N = Steps rem X,
    T = Steps div X,

    Visited = bfs2([{Start, 0}], Grid, #{}, N + 2 * X),

    A = count(Visited, N),
    B = count(Visited, N + X),
    C = count(Visited, N + X * 2),
    K1 = (-3 * A + 4 * B - C),
    K2 = (A - 2 * B + C),

    io:format("~p + ~px + ~px^2~n", [A, K1 / 2, K2 / 2]),
    (2 * A + K1 * T + K2 * T * T) div 2.

read(File) ->
    Grid = tools:read_grid(File),
    [Start] = [S || S := $S <- Grid],
    {Start, Grid}.

bfs2([], _G, Visited, _Nmax) ->
    maps:values(Visited);
bfs2([{_Pos, N} | _Rest], _Grid, Visited, Nmax) when N > Nmax ->
    maps:values(Visited);
bfs2([{Pos, N} | Rest], Grid, Visited, Nmax) ->
    case maps:is_key(Pos, Visited) of
        false ->
            bfs2(Rest ++ neigbours2(Pos, Grid, N + 1), Grid, Visited#{Pos => N}, Nmax);
        true ->
            bfs2(Rest, Grid, Visited, Nmax)
    end.
neigbours2({X, Y}, #{max := {Xmax, Ymax}} = Grid, N) ->
    [
        {Pos, N}
     || {Px, Py} = Pos <- [{X, Y - 1}, {X, Y + 1}, {X - 1, Y}, {X + 1, Y}],
        maps:get({tools:mod(Px, Xmax + 1), tools:mod(Py, Ymax + 1)}, Grid) /= $#
    ].

count(Visited, Steps) ->
    length([V || V <- Visited, (Steps - V) rem 2 == 0, V =< Steps]).
