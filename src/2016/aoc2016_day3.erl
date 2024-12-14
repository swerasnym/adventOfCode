-module(aoc2016_day3).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 3}
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Triangles) ->
    length([T || T <- Triangles, is_triangle(lists:sort(T))]).

star2(Data) ->
    Grid0 = tools:lists_to_grid(Data),
    Grid1 = tools:rotate_grid(Grid0),
    [T1, T2, T3] = tools:grid_to_lists(Grid1),
    length([T || T <- tools:group(3, T1 ++ T2 ++ T3), is_triangle(T)]).

read(File) ->
    tools:read_lines(File, fun tools:parse_integers/1).

is_triangle([A, B, C]) ->
    A + B > C;
is_triangle({A, B, C}) ->
    is_triangle(lists:sort([A, B, C])).
