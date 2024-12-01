-module(aoc2024_day1).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day1_ex.txt", star1, 11},
        {"examples/2024/day1_ex.txt", star2, 31}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 1},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({L1, L2}) ->
    S1 = lists:sort(L1),
    S2 = lists:sort(L2),
    Data = lists:zip(S1, S2),
    lists:sum([abs(I1 - I2) || {I1, I2} <- Data]).

star2({L1, L2}) ->
    C2 = tools:count(L2),
    lists:sum([I1 * maps:get(I1, C2, 0) || I1 <- L1]).

read(File) ->
    lists:unzip(tools:group(2, tools:read_integers(File))).
