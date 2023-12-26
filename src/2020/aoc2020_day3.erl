-module(aoc2020_day3).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2020/data/dayN_ex.txt", star1, unknown},
        % {"2020/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2020, 3},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).
star1(Grid) ->
    toboggan(Grid, {3, 1}).

star2(Grid) ->
    Dirs = [{1, 1}, {3, 1}, {5, 1}, {7, 1}, {1, 2}],
    tools:product([toboggan(Grid, Dir) || Dir <- Dirs]).

read(File) ->
    tools:read_grid(File, #{$# => tree, $. => open}).

toboggan(#{max := {_Xmax, Ymax}} = Grid, {Dx, Dy}) ->
    tools:count(tree, [get_pos(N * Dx, N * Dy, Grid) || N <- lists:seq(0, Ymax div Dy)]).

get_pos(X, Y, #{max := {Xmax, _Ymax}} = Grid) ->
    maps:get({X rem (Xmax + 1), Y}, Grid).
