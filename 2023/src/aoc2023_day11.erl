-module(aoc2023_day11).

-export([run/0, run/2]).

run() ->
    %%{S1, S2} = Res = run(all, "/home/rasmus/repos/adventOfCode/2023/data/day11_ex.txt"),
    {S1, S2} = Res = run(all, aoc_web:get_input_path(2023, 11)),
    io:format("S1: ~p ~nS2: ~p ~n", [S1, S2]),
    Res.

run(Star, File) ->
    Data = read(File),
    case Star of
        star1 ->
            star1(Data);
        star2 ->
            star2(Data);
        _ ->
            Star1 = star1(Data),
            Star2 = star2(Data),
            {Star1, Star2}
    end.

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
