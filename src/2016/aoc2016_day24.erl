-module(aoc2016_day24).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day24_ex.txt", star1, 14},
        {"examples/2016/day24_ex.txt", star2, 20}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 24},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    Interesting = interesting(Map),
    Distances = find_distances(Map, Interesting),
    io:format("~kp~n", [Distances]),
    Positions = maps:keys(Interesting) -- [$0],
    lists:min([total_distance($0, Path, Distances, 0) || Path <- tools:perms(Positions)]).

star2(Map) ->
    Interesting = interesting(Map),
    Distances = find_distances(Map, Interesting),
    Positions = maps:keys(Interesting) -- [$0],
    lists:min([total_distance($0, Path ++ [$0], Distances, 0) || Path <- tools:perms(Positions)]).

read(File) ->
    tools:drop_max(tools:read_grid(File)).

interesting(Map) ->
    #{K => Pos || Pos := K <- Map, K /= $#, K /= $.}.

neighbours(Map) ->
    fun(Pos) ->
        Ns = [aoc_vector:add(Pos, Dp) || Dp <- [{1, 0}, {0, 1}, {0, -1}, {-1, 0}]],
        [N || N <- Ns, maps:get(N, Map, $#) /= $#]
    end.

find_distances(Map, Interesting) ->
    All = [find_distances_from(Map, From, Interesting) || From <- maps:keys(Interesting)],
    maps:from_list(lists:flatten(All)).

distance(To, Visited) ->
    {Dist, _} = maps:get(To, Visited),
    Dist.

find_distances_from(Map, From, Interesting) ->
    Start = maps:get(From, Interesting),
    {no_path, Visited} = aoc_graph:bfs(Start, all, neighbours(Map)),
    [{[From, To], distance(Pos, Visited)} || To := Pos <- Interesting, To /= From].

total_distance(_, [], _, Sum) ->
    Sum;
total_distance(Pos, [Next | Rest], Distances, Sum) ->
    total_distance(Next, Rest, Distances, Sum + maps:get([Pos, Next], Distances)).
