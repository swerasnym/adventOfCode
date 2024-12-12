-module(aoc2024_day12).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day12_ex1.txt", star1, 140},
        {"examples/2024/day12_ex1.txt", star2, 80},
        {"examples/2024/day12_ex2.txt", star1, 772},
        {"examples/2024/day12_ex2.txt", star2, 436},
        {"examples/2024/day12_ex3.txt", star1, 1930},
        {"examples/2024/day12_ex3.txt", star2, 1206}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 12},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    Regions = find_all_regions(maps:keys(Map), #{}, Map, []),
    lists:sum([calculate_price(R, Map) || R <- Regions]).

star2(Map) ->
    Regions = find_all_regions(maps:keys(Map), #{}, Map, []),
    lists:sum([calculate_price2(R, Map) || R <- Regions]).

read(File) ->
    tools:drop_max(tools:read_grid(File)).

neighbours(Pos) ->
    [aoc_vector:add(Pos, Dp) || Dp <- [{1, 0}, {0, 1}, {0, -1}, {-1, 0}]].

calculate_price(Positions, Map) ->
    Type = maps:get(hd(Positions), Map),
    Area = length(Positions),
    Neighbours = lists:flatten([neighbours(P) || P <- Positions]),
    Perimeter = length([N || N <- Neighbours, maps:get(N, Map, other) /= Type]),
    Area * Perimeter.

calculate_price2(Positions, Map) ->
    Type = maps:get(hd(Positions), Map),
    Area = length(Positions),
    Neighbours = lists:flatten([lists:enumerate(neighbours(P)) || P <- Positions]),
    Perimeter = [N || N = {_, Pos} <- Neighbours, maps:get(Pos, Map, other) /= Type],
    Sections = count_sections(lists:sort(Perimeter), #{}, 0),
    Area * Sections.

count_sections([], _, Count) ->
    Count;
count_sections([H = {S, Pos} | Rest], Visited, Count) ->
    case lists:any(fun(P) -> maps:get({S, P}, Visited, false) end, neighbours(Pos)) of
        true ->
            count_sections(Rest, Visited#{H => true}, Count);
        false ->
            count_sections(Rest, Visited#{H => true}, Count + 1)
    end.

find_all_regions([], _InRegion, _Map, Acc) ->
    Acc;
find_all_regions([Pos | Rest], InRegion, Map, Acc) ->
    case maps:get(Pos, InRegion, false) of
        true ->
            find_all_regions(Rest, InRegion, Map, Acc);
        false ->
            Type = maps:get(Pos, Map),
            Region = find_region(Type, [Pos], Map, #{}),
            find_all_regions(Rest, maps:merge(InRegion, Region), Map, [maps:keys(Region) | Acc])
    end.

find_region(_, [], _Map, Visited) ->
    Visited;
find_region(Type, [Pos | Rest], Map, Visited) ->
    case maps:get(Pos, Visited, false) of
        true ->
            find_region(Type, Rest, Map, Visited);
        false ->
            find_region(
                Type,
                Rest ++ [N || N <- neighbours(Pos), maps:get(N, Map, other) == Type],
                Map,
                Visited#{Pos => true}
            )
    end.
