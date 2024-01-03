-module(aoc2023_day17).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2023/day17_ex.txt", star1, 102},
        {"examples/2023/day17_ex.txt", star2, 94}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 17},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(#{max := {Ex, Ey} = End} = Map) ->
    Start = {{0, 0}, start},
    EndFun = fun({Pos, _}) -> Pos == End end,
    Neighbours = fun(PosDir) -> move(PosDir, 1, 3, Map) end,
    Estimate = fun({{X, Y}, _}) -> abs(Ex - X) + abs(Ey - Y) end,
    {Dist, _, _} = aoc_graph:a_star(Start, EndFun, Neighbours, Estimate),
    Dist.

star2(#{max := {Ex, Ey} = End} = Map) ->
    Start = {{0, 0}, start},
    EndFun = fun({Pos, _}) -> Pos == End end,
    Neighbours = fun(PosDir) -> move(PosDir, 4, 10, Map) end,
    Estimate = fun({{X, Y}, _}) -> abs(Ex - X) + abs(Ey - Y) end,
    {Dist, _, _} = aoc_graph:a_star(Start, EndFun, Neighbours, Estimate),
    Dist.

read(File) ->
    tools:read_grid(File, fun(D) -> D - $0 end).

get_neighbour({X, Y}, north) -> {X, Y - 1};
get_neighbour({X, Y}, south) -> {X, Y + 1};
get_neighbour({X, Y}, east) -> {X + 1, Y};
get_neighbour({X, Y}, west) -> {X - 1, Y}.

get_turns(start) ->
    [south, east];
get_turns(north) ->
    [east, west];
get_turns(south) ->
    [east, west];
get_turns(east) ->
    [north, south];
get_turns(west) ->
    [north, south].

move({Pos, Dir}, Min, Max, Map) ->
    [D1, D2] = get_turns(Dir),

    Move1 = fun({D, P}, Direction) ->
        N = get_neighbour(P, Direction),
        Dist = D + maps:get(N, Map, 0),
        {{Dist, {N, Direction}}, {Dist, N}}
    end,

    {New1, _} = lists:mapfoldl(fun(_, A) -> Move1(A, D1) end, {0, Pos}, lists:seq(1, Max)),
    {New2, _} = lists:mapfoldl(fun(_, A) -> Move1(A, D2) end, {0, Pos}, lists:seq(1, Max)),
    New = lists:nthtail(Min - 1, New1) ++ lists:nthtail(Min - 1, New2),
    [N || {_, {Np, _}} = N <- New, maps:is_key(Np, Map)].
