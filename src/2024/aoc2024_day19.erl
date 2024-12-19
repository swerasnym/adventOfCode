-module(aoc2024_day19).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day19_ex.txt", star1, 6},
        {"examples/2024/day19_ex.txt", star2, 16}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 19},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Patterns, Towels}) ->
    Possible = [T || T <- Towels, possible(T, Patterns)],
    length(Possible).

star2({Patterns, Towels}) ->
    {List, _Map1} = lists:mapfoldl(fun(T, M) -> ways(T, Patterns, M) end, #{}, Towels),
    lists:sum(List).

read(File) ->
    [Patterns, Towels] = tools:read_blocks(File),
    {
        string:split(Patterns, ", ", all),
        tools:parse_lines(Towels)
    }.

possible("", _) ->
    true;
possible(Towel, Patterns) ->
    SubT = [Towel -- P || P <- Patterns, lists:prefix(P, Towel)],
    lists:any(fun(T) -> possible(T, Patterns) end, SubT).

ways("", _, Map) ->
    {1, Map};
ways(Towel, Patterns, Map) ->
    case maps:get(Towel, Map, none) of
        none ->
            SubT = [Towel -- P || P <- Patterns, lists:prefix(P, Towel)],
            {List, Map1} = lists:mapfoldl(fun(T, M) -> ways(T, Patterns, M) end, Map, SubT),
            Sum = lists:sum(List),
            {Sum, Map1#{Towel => Sum}};
        N ->
            {N, Map}
    end.
