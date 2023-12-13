-module(aoc2023_day11).
-behaviour(aoc_solution).

-export([run/0, run/2]).
%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2023, 11}}).

run() ->
    %% aoc_solution:run(?MODULE, all, "2023/data/day12_ex.txt").
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Grid) ->
    Lists1 = tools:grid_to_lists(Grid),
    Lists2 = lists:append([check_list(R) || R <- Lists1]),
    Grid1 = tools:lists_to_grid(Lists2),
    % tools:print_grid(Grid1),
    Lists3 = tools:grid_to_lists(tools:rotate_grid(Grid1, cw)),
    Lists4 = lists:append([check_list(R) || R <- Lists3]),
    Grid2 = tools:lists_to_grid(Lists4),
    %% tools:print_grid(tools:rotate_grid(Grid2,ccw)),
    Stars = [Star || Star := $# <- Grid2],

    lists:sum([
        abs(X1 - X2) + abs(Y1 - Y2)
     || {X1, Y1} = S1 <- Stars, {X2, Y2} = S2 <- Stars, S1 > S2
    ]).

star2(Grid) ->
    Lists1 = tools:grid_to_lists(Grid),
    EmptyRows = [N || {N, R} <- lists:enumerate(0, Lists1), all_empty(R)],

    Lists2 = tools:grid_to_lists(tools:rotate_grid(Grid, cw)),
    EmptyCols = [N || {N, R} <- lists:enumerate(0, Lists2), all_empty(R)],

    Amount = 1000000 - 1,
    R = fun(N) -> add(N, EmptyRows, Amount) end,
    C = fun(N) -> add(N, EmptyCols, Amount) end,
    Stars = [{C(X), R(Y)} || {X, Y} := $# <- Grid],
    % %io:format("~w", [Stars]),

    lists:sum([
        abs(X1 - X2) + abs(Y1 - Y2)
     || {X1, Y1} = S1 <- Stars, {X2, Y2} = S2 <- Stars, S1 > S2
    ]).

read(File) ->
    tools:read_grid(File).

all_empty([]) ->
    true;
all_empty([$. | Rest]) ->
    all_empty(Rest);
all_empty(_) ->
    false.

check_list(L) ->
    case all_empty(L) of
        true ->
            [L, L];
        false ->
            [L]
    end.

add(N, List, Amount) ->
    N + length([L || L <- List, L < N]) * Amount.
