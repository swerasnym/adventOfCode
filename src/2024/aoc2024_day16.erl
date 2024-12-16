-module(aoc2024_day16).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day16_ex1.txt", star1, 7036},
        {"examples/2024/day16_ex1.txt", star2, 45},
        {"examples/2024/day16_ex2.txt", star1, 11048},
        {"examples/2024/day16_ex2.txt", star2, 64}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 16},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    [Start] = [P || P := $S <- Map],
    {Cost, _End, _Visited} = aoc_graph:dijkstra({Start, east}, is_end(Map), neigbours(Map)),
    %io:format("~p~n", [aoc_graph:get_path(End, Visited)]),
    Cost.

star2(Map) ->
    [Start] = [P || P := $S <- Map],
    [End] = [P || P := $E <- Map],
    {_, _, Visited} = aoc_graph:dijkstra({Start, east}, is_end(Map), neigbours(Map)),
    Ends = [{End, D} || D <- [north, south, east, west], maps:is_key({End, D}, Visited)],
    Paths = [aoc_graph:get_multiple_paths(E, Visited) || E <- Ends],
    Positions = [P || {_, {P, _}} <- lists:flatten(Paths)],
    length(lists:usort(Positions)).

read(File) ->
    tools:read_grid(File).

neigbours(Map) ->
    fun({Pos, Dir}) ->
        Turn = [{1000, {Pos, D}} || D <- get_turns(Dir)],
        [
            {Score, State}
         || {Score, {P, _} = State} <- [{1, {get_neighbour(Pos, Dir), Dir}} | Turn],
            maps:get(P, Map) /= $#
        ]
    end.

is_end(Map) ->
    fun({Pos, _Dir}) ->
        maps:get(Pos, Map) == $E
    end.

get_neighbour({X, Y}, north) -> {X, Y - 1};
get_neighbour({X, Y}, south) -> {X, Y + 1};
get_neighbour({X, Y}, east) -> {X + 1, Y};
get_neighbour({X, Y}, west) -> {X - 1, Y}.

get_turns(north) ->
    [east, west];
get_turns(south) ->
    [east, west];
get_turns(east) ->
    [north, south];
get_turns(west) ->
    [north, south].
