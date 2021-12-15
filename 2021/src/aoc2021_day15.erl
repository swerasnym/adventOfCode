-module(aoc2021_day15).

-export([run/2, profile/3]).

run(Star, File) ->
    Data = read(File),
    case Star of
        star1 ->
            star1(Data);
        star2 ->
            star2(Data);
        _ ->
            Star1 = star1(Data),
            Star2 = star2(Data),
            {Star1, Star2}
    end.

profile(Star, File, Times) ->
    Data = read(File),

    case Star of
        star1 ->
            profile(fun() -> star1(Data) end, Times);
        star2 ->
            profile(fun() -> star2(Data) end, Times);
        _ ->
            Star1 = profile(fun() -> star1(Data) end, Times),
            Star2 = profile(fun() -> star2(Data) end, Times),
            {Star1, Star2}
    end.

profile(F, Times) ->
    Expected = F(),
    Results =
        [begin
             {Time, Expected} = timer:tc(F),
             Time
         end
         || _ <- lists:seq(1, Times)],
    {Expected, lists:sum(Results) / Times / 1000}.

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

neigbours({X, Y}) ->
    [{X, Y + 1}, {X + 1, Y}, {X, Y - 1},  {X - 1, Y}].

bfs(End, [{Risk, End} | _], _Map) ->
    Risk;
bfs(End, [{Risk, Pos} | Rest], Map) ->
    case maps:get(Pos, Map, visited) of
        visited ->
            bfs(End, Rest, Map);
        _ ->
            New = [{Risk + NRisk, N}
                   || N <- neigbours(Pos), visited /= (NRisk = maps:get(N, Map, visited))],
            bfs(End, lists:umerge(Rest, lists:sort(New)), Map#{Pos => visited})
    end.

%% astar preforms worse than bfs for some reason...
%% astar(End, [{_E, Risk, End}| _], _Map) ->
%%     Risk;
%% astar({Mx, My} = End, [{_E, Risk, Pos} | Rest], Map) ->
%%     case maps:get(Pos, Map, visited) of
%%         visited ->

%%             astar(End, Rest, Map);
%%         _ ->
%%             New = [{Risk + NRisk + (Mx - Nx) + (My - Ny),
%% 		    Risk + NRisk,
%% 		    N}
%%                    || {Nx, Ny} = N <- neigbours(Pos), visited /= (NRisk = maps:get(N, Map, visited))],
%%             astar(End, lists:umerge(Rest, lists:sort(New)), Map#{Pos => visited})
%%     end.

build_map(#{max := {Mx, My}} = Map) ->
    Translated =
        [{tools:translate_grid(Map, {(Mx + 1) * X, (My + 1) * Y}), X + Y}
         || X <- lists:seq(0, 4), Y <- lists:seq(0, 4)],
    NewMaps = lists:map(fun update/1, Translated),
    F = fun(V, Acc) -> maps:merge_with(fun merge/3, V, Acc) end,
    lists:foldl(F, #{}, NewMaps).

update({Map, Mod}) ->
    F = fun (max, V) ->
                V;
            (_K, V) ->
                (V + Mod - 1) rem 9 + 1
        end,
    maps:map(F, Map).

merge(max, V1, V2) when V1 > V2 ->
    V1;
merge(max, V1, V2) when V2 > V1 ->
    V2.
