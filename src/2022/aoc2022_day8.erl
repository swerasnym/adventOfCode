-module(aoc2022_day8).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 8}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    tools:read_grid(File, fun(Pos, Char) -> {Pos, Char - $0} end).

star1(Grid0) ->
    Grid1 = tools:rotate_grid(Grid0),
    Grid2 = tools:rotate_grid(Grid1),
    Grid3 = tools:rotate_grid(Grid2),

    Dir0 = tools:grid_to_lists(Grid0),
    Dir1 = tools:grid_to_lists(Grid1),
    Dir2 = tools:grid_to_lists(Grid2),
    Dir3 = tools:grid_to_lists(Grid3),

    Dirs = Dir0 ++ Dir1 ++ Dir2 ++ Dir3,

    Vis = lists:flatten([visible(Line, -1, []) || Line <- Dirs]),
    length(lists:uniq(Vis)).

star2(Grid0) ->
    Scores = [score(P, Grid0) || P = {_, _} <- maps:keys(Grid0)],
    lists:max(Scores).

visible([{P, H} | Rest], Max, Acc) when H > Max ->
    visible(Rest, H, [P | Acc]);
visible([_ | Rest], Max, Acc) ->
    visible(Rest, Max, Acc);
visible([], _, Acc) ->
    Acc.

distance([{_, H} | Rest], Height, Acc) when H < Height ->
    distance(Rest, Height, Acc + 1);
distance([{_, H} | _], Height, Acc) when H >= Height ->
    Acc + 1;
distance(_, _, Acc) ->
    Acc.

get_lines({PX, PY}, Grid) ->
    List = maps:to_list(Grid),

    D1 = [V || {{X, Y}, V} <- List, X == PX, Y < PY],
    D2 = [V || {{X, Y}, V} <- List, X == PX, Y > PY],
    D3 = [V || {{X, Y}, V} <- List, X < PX, Y == PY],
    D4 = [V || {{X, Y}, V} <- List, X > PX, Y == PY],
    [tools:reverse_sort(D1), lists:sort(D2), tools:reverse_sort(D3), lists:sort(D4)].

score(Pos, Grid) ->
    {_, H} = maps:get(Pos, Grid),

    D = [distance(Line, H, 0) || Line <- get_lines(Pos, Grid)],
    tools:product(D).
