-module(aoc2024_day10).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day10_ex.txt", star1, 36},
        {"examples/2024/day10_ex.txt", star2, 81}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 10},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    Tops = [T || T := 9 <- Map],
    Bottoms = [T || T := 0 <- Map],
    Counts = bfs(Tops, Map, #{Pos => [Pos] || Pos <- Tops}, #{}, fun update_count1/3),
    lists:sum([length(maps:get(Pos, Counts, [])) || Pos <- Bottoms]).

star2(Map) ->
    Tops = [T || T := 9 <- Map],
    Bottoms = [T || T := 0 <- Map],
    Counts = bfs(Tops, Map, #{Pos => 1 || Pos <- Tops}, #{}, fun update_count2/3),
    lists:sum([maps:get(Pos, Counts, 0) || Pos <- Bottoms]).

read(File) ->
    tools:read_grid(File, fun(V) -> V - $0 end).

move_down(Pos, Map) ->
    V = maps:get(Pos, Map),
    Dp = [{1, 0}, {0, 1}, {0, -1}, {-1, 0}],
    Neighbours = [aoc_vector:add(Pos, D) || D <- Dp],
    [N || N <- Neighbours, V - maps:get(N, Map, V) == 1].

bfs([], _, Count, _, _) ->
    Count;
bfs([Pos | Rest], Map, Count, Visited, Update) ->
    case maps:get(Pos, Visited, false) of
        false ->
            Add = maps:get(Pos, Count),
            Down = move_down(Pos, Map),
            bfs(Rest ++ Down, Map, Update(Down, Add, Count), Visited#{Pos => true}, Update);
        true ->
            bfs(Rest, Map, Count, Visited, Update)
    end.

update_count1(N, Add, Count) ->
    maps:merge(Count, #{Pos => lists:usort(Add ++ maps:get(Pos, Count, [])) || Pos <- N}).

update_count2(N, Add, Count) ->
    maps:merge(Count, #{Pos => Add + maps:get(Pos, Count, 0) || Pos <- N}).
