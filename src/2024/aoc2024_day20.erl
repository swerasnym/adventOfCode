-module(aoc2024_day20).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, star2/2, read/1]).

info() ->
    Examples = [
        {"examples/2024/day20_ex.txt", {star1, 10}, 10},
        {"examples/2024/day20_ex.txt", {star2, 50}, 285}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 20},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).
star1(Map) ->
    star1(Map, 100).

star1(Map, MinSave) ->
    Times = find_cheats(Map, 2),
    lists:sum([C || T := C <- Times, T >= MinSave]).
star2(Map) ->
    star2(Map, 100).

star2(Map, MinSave) ->
    Times = find_cheats(Map, 20),
    lists:sum([C || T := C <- Times, T >= MinSave]).

read(File) ->
    tools:read_grid(File).

neighbours(Map) ->
    fun(Pos) ->
        Dn = [aoc_vector:add(Pos, Dp) || Dp <- [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]],
        [{1, D} || D <- Dn, maps:get(D, Map, $#) /= $#]
    end.

dist({X1, Y1}, {X2, Y2}) ->
    abs(X2 - X1) + abs(Y2 - Y1).

is_end(Map) ->
    fun(Pos) ->
        maps:get(Pos, Map) == $E
    end.

find_cheats(Map, TimeLimit) ->
    [Start] = [P || P := $S <- Map],
    {_, End, Visited} = aoc_graph:dijkstra(Start, is_end(Map), neighbours(Map)),
    InPath = aoc_graph:get_nodes_distances_in_shortest_paths(End, Visited),
    %% sanity check that there is only one path...
    true = maps:size(InPath) == maps:size(Visited),
    Cheats = [saved(P1, D1, InPath, TimeLimit) || P1 := D1 <- InPath],
    tools:count(lists:flatten(Cheats)).

saved(P1, D1, InPath, TimeLimit) ->
    Ends = [
        aoc_vector:add(P1, {X, Y})
     || X <- lists:seq(0, TimeLimit), Y <- lists:seq(-TimeLimit + X, TimeLimit - X)
    ],
    [
        abs(D1 - maps:get(P2, InPath)) - dist(P1, P2)
     || P2 <- Ends, P1 < P2, maps:is_key(P2, InPath)
    ].
