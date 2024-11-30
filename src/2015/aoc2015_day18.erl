-module(aoc2015_day18).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, star2/2, read/1]).

info() ->
    Examples = [
        {"examples/2015/day18_ex.txt", {star1, 4}, 4},
        {"examples/2015/day18_ex.txt", {star2, 5}, 17}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 18},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Grid) ->
    star1(Grid, 100).

star1(Grid, Steps) ->
    Result = simulate(Grid, Steps, #{}),
    tools:count($#, Result).

star2(Grid) ->
    star2(Grid, 100).

star2(Grid = #{max := {X, Y}}, Steps) ->
    Permanent = #{{0, 0} => $#, {X, 0} => $#, {0, Y} => $#, {X, Y} => $#},
    Result = simulate(maps:merge(Grid, Permanent), Steps, Permanent),
    tools:count($#, Result).

read(File) ->
    tools:read_grid(File).

simulate(Grid, 0, _) ->
    Grid;
simulate(Grid, Steps, Permanent) ->
    Next = #{Pos => update(Pos, Status, Grid) || Pos = {_, _} := Status <- Grid},
    simulate(maps:merge(Next, Permanent), Steps - 1, Permanent).

update(Pos, Status, Grid) ->
    case {Status, neighbours_on(Pos, Grid)} of
        {$#, N} when N == 2; N == 3 ->
            $#;
        {$., 3} ->
            $#;
        _ ->
            $.
    end.

neighbours_on({X, Y}, Grid) ->
    tools:count($#, [
        maps:get({X + Dx, Y + Dy}, Grid, $.)
     || Dx <- lists:seq(-1, 1), Dy <- lists:seq(-1, 1), {Dx, Dy} /= {0, 0}
    ]).
