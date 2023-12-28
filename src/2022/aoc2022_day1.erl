-module(aoc2022_day1).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 1}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    lists:max([lists:sum(X) || X <- Data]).

star2(Data) ->
    Sums = tools:dsort([lists:sum(X) || X <- Data]),
    lists:sum(lists:sublist(Sums, 3)).

read(File) ->
    tools:read_blocks(File, fun tools:parse_integers/1).
