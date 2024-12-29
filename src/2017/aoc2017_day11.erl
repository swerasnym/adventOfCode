-module(aoc2017_day11).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, [ne, ne, ne]}, star1, 3},
        {{data, [ne, ne, sw, sw]}, star1, 0},
        {{data, [ne, ne, sw, sw]}, star2, 2},
        {{data, [ne, ne, s, s]}, star1, 2},
        {{data, [se, sw, se, sw, sw]}, star1, 3},
        {{data, [se, sw, se, sw, sw]}, star2, 3}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 11},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Directions) ->
    End = lists:foldl(fun move/2, {0, 0}, Directions),
    dist(End).

star2(Directions) ->
    {_, Max} = lists:foldl(fun move2/2, {{0, 0}, 0}, Directions),
    Max.

read(File) ->
    [list_to_atom(D) || D <- string:split(tools:read_string(File), ",", all)].

move2(D, {Pos, Max}) ->
    Next = move(D, Pos),
    {Next, max(Max, dist(Next))}.

move(n, {X, Y}) -> {X, Y - 2};
move(ne, {X, Y}) -> {X + 1, Y - 1};
move(se, {X, Y}) -> {X + 1, Y + 1};
move(s, {X, Y}) -> {X, Y + 2};
move(sw, {X, Y}) -> {X - 1, Y + 1};
move(nw, {X, Y}) -> {X - 1, Y - 1}.

dist({X, Y}) ->
    Dx = abs(X),
    Dy = abs(Y),
    Dx + max(0, (Dy - Dx) div 2).
