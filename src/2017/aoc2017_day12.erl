-module(aoc2017_day12).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day12_ex.txt", star1, 6},
        {"examples/2017/day12_ex.txt", star2, 2}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 12},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    {no_path, Reachable} = aoc_graph:bfs(0, none, neighbours(Map)),
    maps:size(Reachable).

star2(Map) ->
    count_groups(maps:keys(Map), neighbours(Map), 0).

read(File) ->
    maps:from_list(tools:read_lines(File, fun parse_line/1)).

parse_line(Line) ->
    [From | To] = tools:parse_integers(Line, " <->,"),
    {From, To}.

neighbours(Map) ->
    fun(Pos) ->
        maps:get(Pos, Map)
    end.

count_groups([], _, Count) ->
    Count;
count_groups([Hd | Rest], Neighbours, Count) ->
    {no_path, Reachable} = aoc_graph:bfs(Hd, none, Neighbours),
    count_groups(Rest -- maps:keys(Reachable), Neighbours, Count + 1).
