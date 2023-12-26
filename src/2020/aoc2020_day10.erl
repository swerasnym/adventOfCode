-module(aoc2020_day10).
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
        problem => {2020, 10},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Sorted) ->
    #{1 := One, 3 := Three} = tools:count(diffrences(Sorted)),
    One * Three.

star2(Sorted) ->
    Diff = diffrences(Sorted),
    tools:product([multiplicity(Rep) || Rep <- count_repeeted_ones(Diff)]).

read(File) ->
    Sorted = tools:read_integers(File, sort),
    [0] ++ Sorted ++ [lists:last(Sorted) + 3].

diffrences(Sorted) ->
    diffrences(Sorted, []).

diffrences([_], Diffrences) ->
    lists:reverse(Diffrences);
diffrences([A, B | Sorted], Diffrences) ->
    diffrences([B | Sorted], [B - A | Diffrences]).

count_repeeted_ones(Diff) ->
    count_repeeted_ones(Diff, 0, []).

count_repeeted_ones([], _Count, Res) ->
    lists:reverse(Res);
count_repeeted_ones([1 | Diff], Count, Res) ->
    count_repeeted_ones(Diff, Count + 1, Res);
count_repeeted_ones([3 | Diff], Count, Res) ->
    count_repeeted_ones(Diff, 0, [Count | Res]).

multiplicity(0) ->
    1;
multiplicity(1) ->
    1;
multiplicity(2) ->
    2;
multiplicity(3) ->
    4;
multiplicity(4) ->
    7.
