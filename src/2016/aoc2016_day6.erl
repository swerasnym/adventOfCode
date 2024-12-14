-module(aoc2016_day6).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day6_ex.txt", star1, "easter"},
        {"examples/2016/day6_ex.txt", star2, "advent"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 6},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Grid0 = tools:lists_to_grid(Data),
    Grid1 = tools:rotate_grid(Grid0, cw),
    Rotated = tools:grid_to_lists(Grid1),
    [most_common(L) || L <- Rotated].

star2(Data) ->
    Grid0 = tools:lists_to_grid(Data),
    Grid1 = tools:rotate_grid(Grid0, cw),
    Rotated = tools:grid_to_lists(Grid1),
    [least_common(L) || L <- Rotated].

read(File) ->
    tools:read_lines(File).

most_common(Line) ->
    Counts = tools:count(Line),
    {_, L} = lists:max([{C, L} || L := C <- Counts]),
    L.

least_common(Line) ->
    Counts = tools:count(Line),
    {_, L} = lists:min([{C, L} || L := C <- Counts]),
    L.
