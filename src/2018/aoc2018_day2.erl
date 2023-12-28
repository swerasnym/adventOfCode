-module(aoc2018_day2).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2018/day2_ex.txt", star1, 12},
        {"examples/2018/day2_ex2.txt", star2, "fgij"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 2},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Counts = [{L, tools:count(L)} || L <- Data],
    TwoTimes = [L || {L, C} <- Counts, check(2, C)],
    ThreeTimes = [L || {L, C} <- Counts, check(3, C)],
    length(TwoTimes) * length(ThreeTimes).

star2(Data) ->
    [Common] = [C || A <- Data, B <- Data, A < B, {C, true} <- common(A, B, [], 0)],
    Common.

read(File) ->
    tools:read_lines(File).

check(N, Counts) ->
    lists:member(N, maps:values(Counts)).

common([], [], Common, 1) ->
    [{lists:reverse(Common), true}];
common(_, _, Common, 2) ->
    [{Common, false}];
common([A | RestA], [A | RestB], Common, Differ) ->
    common(RestA, RestB, [A | Common], Differ);
common([_ | RestA], [_ | RestB], Common, Differ) ->
    common(RestA, RestB, Common, Differ + 1).
