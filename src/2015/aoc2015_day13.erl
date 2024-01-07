-module(aoc2015_day13).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2015/day13_ex.txt", star1, 330},
        {"examples/2015/day13_ex.txt", star2, 286}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 13},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    [First | Others] = maps:keys(Map),
    lists:max([arrange([First | P] ++ [First], Map, 0) || P <- tools:perms(Others)]).

star2(Map) ->
    Others = maps:keys(Map),
    lists:max([arrange([me | P] ++ [me], Map, 0) || P <- tools:perms(Others)]).

read(File) ->
    Distances = tools:read_multiple_formats(
        File, "~s would ~a ~d happiness units by sitting next to ~s"
    ),
    Edges = lists:flatten([{A, lists:droplast(B), value(T, D)} || [A, T, D, B] <- Distances]),
    Map = maps:groups_from_list(fun({K, _, _}) -> K end, fun({_, A, B}) -> {A, B} end, Edges),
    maps:map(fun(_, V) -> maps:from_list(V) end, Map).

value(gain, V) ->
    V;
value(lose, V) ->
    -V.

arrange([_], _, Sum) ->
    Sum;
arrange([A, B | Rest], Map, Sum) ->
    NewSum = Sum + maps:get(A, maps:get(B, Map, #{}), 0) + maps:get(B, maps:get(A, Map, #{}), 0),
    arrange([B | Rest], Map, NewSum).
