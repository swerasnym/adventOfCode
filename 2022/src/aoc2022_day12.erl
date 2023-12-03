-module(aoc2022_day12).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day12.txt"),
    io:format("S1: ~p ~nS2: ~p ~n", [S1, S2]),
    Res.

run(Star, File) ->
    Data = read(File),
    Res =
        case Star of
            star1 ->
                star1(Data);
            star2 ->
                star2(Data);
            _ ->
                Star1 = star1(Data),
                Star2 = star2(Data),
                {Star1, Star2}
        end,
    erlang:erase(),
    Res.

read(File) ->
    tools:read_grid(File).

star1(Map) ->
    erlang:erase(),
    [{Start, $S}] = maps:to_list(maps:filter(fun(_K, V) -> V == $S end, Map)),
    [{End, $E}] = maps:to_list(maps:filter(fun(_K, V) -> V == $E end, Map)),
    search_up([{Start, $a, 0}], End, Map#{Start => $a, End => $z}).

star2(Map) ->
    erlang:erase(),
    [{Start, $S}] = maps:to_list(maps:filter(fun(_K, V) -> V == $S end, Map)),
    [{End, $E}] = maps:to_list(maps:filter(fun(_K, V) -> V == $E end, Map)),
    search_down([{End, $z, 0}], Map#{Start => $a, End => $z}).

neigbours({X, Y}) ->
    [
        {X + Dx, Y + Dy}
     || Dx <- [-1, 0, 1], Dy <- [-1, 0, 1], {Dx, Dy} /= {0, 0}, Dx * Dy == 0
    ].

search_up([{End, _Height, Steps} | _Rest], End, _Map) ->
    Steps;
search_up([{Pos, Height, Steps} | Rest], End, Map) ->
    N = [
        {P, H, Steps + 1}
     || P <- neigbours(Pos),
        (H = maps:get(P, Map, $a + 100)) =< Height + 1,
        put(P, visited) /= visited
    ],
    search_up(Rest ++ N, End, Map).

search_down([{_End, $a, Steps} | _Rest], _Map) ->
    Steps;
search_down([{Pos, Height, Steps} | Rest], Map) ->
    N = [
        {P, H, Steps + 1}
     || P <- neigbours(Pos),
        (H = maps:get(P, Map, -100)) + 1 >= Height,
        put(P, visited) /= visited
    ],
    search_down(Rest ++ N, Map).
