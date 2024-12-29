-module(aoc2015_day9).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2015/day9_ex.txt", star1, 605},
        {"examples/2015/day9_ex.txt", star2, 982}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 9},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    lists:min([distance(P, Map, 0) || P <- tools:perms(maps:keys(Map))]).

star2(Map) ->
    lists:max([distance(P, Map, 0) || P <- tools:perms(maps:keys(Map))]).

read(File) ->
    Distances = tools:read_multiple_formats(File, "~s to ~s = ~d"),
    Edges = lists:flatten([[{A, {B, D}}, {B, {A, D}}] || [A, B, D] <- Distances]),
    Map = tools:group_kv(Edges),
    #{K => maps:from_list(V) || K := V <- Map}.

distance([_], _Map, Acc) ->
    Acc;
distance([F, T | Rest], Map, Acc) ->
    distance([T | Rest], Map, Acc + maps:get(T, maps:get(F, Map))).
