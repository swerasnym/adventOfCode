-module(aoc2022_day14).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 14}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    Lines = tools:read_lines(File),
    [tools:group(2, tools:parse_integers(L, ", ->")) || L <- Lines].

star1(Data) ->
    Walls = [{{500, 0}, $+}] ++ [wall(D, []) || D <- Data],
    Grid = maps:from_list(lists:flatten(Walls)),
    % tools:print_grid(Grid),
    {{Xmin, Xmax}, {Ymin, Ymax}} = tools:min_max_grid(Grid),
    L = [{{Xmin - 1, Y}, $A} || Y <- lists:seq(Ymin, Ymax)],
    R = [{{Xmax + 1, Y}, $A} || Y <- lists:seq(Ymin, Ymax)],
    B = [{{X, Ymax + 1}, $A} || X <- lists:seq(Xmin - 1, Xmax + 1)],
    Grid2 = maps:from_list(L ++ R ++ B ++ lists:flatten(Walls)),
    Grid3 = fall(Grid2, -1),

    %% tools:print_grid(Grid3),
    tools:count($O, Grid3).

star2(Data) ->
    Walls = [{{500, 0}, $+}] ++ [wall(D, []) || D <- Data],
    Grid = maps:from_list(lists:flatten(Walls)),
    {{_Xmin, _Xmax}, {_Ymin, Ymax}} = tools:min_max_grid(Grid),
    Grid2 = fall(Grid, Ymax + 1),
    %    tools:print_grid(Grid3),
    tools:count($O, Grid2).

wall([{X, Y1} | Rest = [{X, Y2} | _]], Acc) ->
    wall(Rest, Acc ++ [{{X, Y}, $#} || Y <- lists:seq(Y1, Y2, (Y2 - Y1) div abs(Y2 - Y1))]);
wall([{X1, Y} | Rest = [{X2, Y} | _]], Acc) ->
    wall(Rest, Acc ++ [{{X, Y}, $#} || X <- lists:seq(X1, X2, (X2 - X1) div abs(X2 - X1))]);
wall([_], Acc) ->
    Acc.

fall(Grid, B) ->
    fall({500, 0}, Grid, B).

fall({X, B}, Grid, B) ->
    fall(Grid#{{X, B} => $O}, B);
fall({X, Y}, Grid, B) ->
    case
        {
            maps:get({X - 1, Y + 1}, Grid, empty),
            maps:get({X, Y + 1}, Grid, empty),
            maps:get({X + 1, Y + 1}, Grid, empty)
        }
    of
        {_, empty, _} ->
            fall({X, Y + 1}, Grid, B);
        {empty, _, _} ->
            fall({X - 1, Y + 1}, Grid, B);
        {_, _, empty} ->
            fall({X + 1, Y + 1}, Grid, B);
        {_, $A, _} ->
            Grid;
        {$A, _, _} ->
            Grid;
        {_, _, $A} ->
            Grid;
        _ when X == 500, Y == 0 ->
            Grid#{{X, Y} => $O};
        _ ->
            fall(Grid#{{X, Y} => $O}, B)
    end.
