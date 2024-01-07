-module(aoc2015_day8).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 8},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Strings) ->
    lists:sum([diff(S) || S <- Strings]).
star2(Strings) ->
    lists:sum([diff2(S) || S <- Strings]).

read(File) ->
    tools:read_lines(File).

diff(String) ->
    length(String) - length(tools:as_term(String)).

diff2(String) ->
    Encoded = lists:flatten(io_lib:format("~p", [String])),
    length(Encoded) - length(String).
