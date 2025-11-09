-module(aoc2021_day15).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 15},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(#{max := Max} = Map) ->
    Start = {0, 0},
    Goal = Max,
    bfs(Goal, [{0, Start}], Map).

star2(Map) ->
    Start = {0, 0},
    #{max := BigMax} = BigMap = build_map(Map),
    %% astar(BigMax, [{0, 0, Start}], BigMap).
    bfs(BigMax, [{0, Start}], BigMap).

read(File) ->
    tools:read_grid(File, fun(V) -> V - $0 end).

neighbours({X, Y}) ->
    [{X, Y + 1}, {X + 1, Y}, {X, Y - 1}, {X - 1, Y}].

bfs(End, [{Risk, End} | _], _Map) ->
    Risk;
bfs(End, [{Risk, Pos} | Rest], Map) ->
    case maps:get(Pos, Map, visited) of
        visited ->
            bfs(End, Rest, Map);
        _ ->
            New = [
                % eqwalizer:ignore
                {Risk + NRisk, N}
             || N <- neighbours(Pos), visited /= (NRisk = maps:get(N, Map, visited))
            ],
            bfs(End, lists:umerge(Rest, lists:sort(New)), Map#{Pos => visited})
    end.

build_map(#{max := {Mx, My}} = Map) ->
    Translated =
        [
            {tools:translate_grid(Map, {(Mx + 1) * X, (My + 1) * Y}), X + Y}
         || X <- lists:seq(0, 4), Y <- lists:seq(0, 4)
        ],
    NewMaps = lists:map(fun update/1, Translated),
    F = fun(V, Acc) -> maps:merge_with(fun merge/3, V, Acc) end,
    lists:foldl(F, #{}, NewMaps).

update({Map, Mod}) ->
    F = fun
        (max, V) ->
            V;
        (_K, V) ->
            (V + Mod - 1) rem 9 + 1
    end,
    maps:map(F, Map).

merge(max, V1, V2) when V1 > V2 ->
    V1;
merge(max, V1, V2) when V2 > V1 ->
    V2.
