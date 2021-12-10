-module(aoc2021_day9).

-export([run/2]).

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

star1(Data) ->
    Map = tools:drop_max(Data),
    Low = lists:map(fun(V) -> {V, low(V, Map)} end, maps:to_list(Map)),
    lists:sum([V - $0 + 1 || {{_, V}, true} <- Low]).

star2(Data) ->
    Map = tools:drop_max(Data),
    Low = lists:map(fun(V) -> {V, low(V, Map)} end, maps:to_list(Map)),
    Lowpoints = [K || {{K, _V}, true} <- Low],
    Sizes = [basin([P], [], Map) || P <- Lowpoints],
    [A, B, C | _] =
        lists:reverse(
            lists:sort(Sizes)),
    A * B * C.

read(File) ->
    tools:read_grid(File).

low({Point, Value}, Map) ->
    lists:all(fun(V) -> V end, [Value < maps:get(N, Map, $9) || N <- neigbours(Point)]).

neigbours({X, Y}) ->
    [{X, Y - 1}, {X, Y + 1}, {X - 1, Y}, {X + 1, Y}].

basin([], In, _Map) ->
    length(In);
basin([Point | Rest], In, Map) ->
    case maps:get(Point, Map, $9) /= $9 of
        true ->
            basin(Rest ++ neigbours(Point), [Point | In], maps:update(Point, $9, Map));
        false ->
            basin(Rest, In, Map)
    end.
