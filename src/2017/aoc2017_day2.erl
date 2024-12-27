-module(aoc2017_day2).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day2_ex.txt", star1, 18},
        {"examples/2017/day2_ex2.txt", star2, 9}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 2},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Rows) ->
    lists:sum([checksum(R) || R <- Rows]).

star2(Rows) ->
    lists:sum([divides(R) || R <- Rows]).

read(File) ->
    tools:read_lines(File, fun tools:parse_integers/1).

checksum(Row) ->
    lists:max(Row) - lists:min(Row).

divides(Row) ->
    [R] = [A div B || A <- Row, B <- Row, A /= B, A rem B == 0],
    R.
