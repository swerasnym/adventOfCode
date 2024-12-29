-module(aoc2022_day12).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 12}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

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

neighbours({X, Y}) ->
    [
        {X + Dx, Y + Dy}
     || Dx <- [-1, 0, 1], Dy <- [-1, 0, 1], {Dx, Dy} /= {0, 0}, Dx * Dy == 0
    ].

search_up([{End, _Height, Steps} | _Rest], End, _Map) ->
    Steps;
search_up([{Pos, Height, Steps} | Rest], End, Map) ->
    N = [
        {P, H, Steps + 1}
     || P <- neighbours(Pos),
        (H = maps:get(P, Map, $a + 100)) =< Height + 1,
        erlang:put(P, visited) /= visited
    ],
    search_up(Rest ++ N, End, Map).

search_down([{_End, $a, Steps} | _Rest], _Map) ->
    Steps;
search_down([{Pos, Height, Steps} | Rest], Map) ->
    N = [
        {P, H, Steps + 1}
     || P <- neighbours(Pos),
        (H = maps:get(P, Map, -100)) + 1 >= Height,
        erlang:put(P, visited) /= visited
    ],
    search_down(Rest ++ N, Map).
