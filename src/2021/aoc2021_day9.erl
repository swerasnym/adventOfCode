-module(aoc2021_day9).
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
        problem => {2021, 9},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Map = tools:drop_max(Data),
    Low = lists:map(fun(V) -> {V, low(V, Map)} end, maps:to_list(Map)),
    lists:sum([V - $0 + 1 || {{_, V}, true} <- Low]).

star2(Data) ->
    Map = tools:drop_max(Data),
    Low = lists:map(fun(V) -> {V, low(V, Map)} end, maps:to_list(Map)),
    Lowpoints = [K || {{K, _V}, true} <- Low],
    Sizes = [basin([P], [], Map) || P <- Lowpoints],
    [A, B, C | _] = lists:reverse(lists:sort(Sizes)),
    A * B * C.

read(File) ->
    tools:read_grid(File).

low({Point, Value}, Map) ->
    lists:all(fun(V) -> V end, [Value < maps:get(N, Map, $9) || N <- neighbours(Point)]).

neighbours({X, Y}) ->
    [{X, Y - 1}, {X, Y + 1}, {X - 1, Y}, {X + 1, Y}].

basin([], In, _Map) ->
    length(In);
basin([Point | Rest], In, Map) ->
    case maps:get(Point, Map, $9) /= $9 of
        true ->
            basin(Rest ++ neighbours(Point), [Point | In], maps:update(Point, $9, Map));
        false ->
            basin(Rest, In, Map)
    end.
