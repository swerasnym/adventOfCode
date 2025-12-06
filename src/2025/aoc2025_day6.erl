-module(aoc2025_day6).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2025/day6_ex.txt", star1, 4277556},
        {"examples/2025/day6_ex.txt", star2, 3263827}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2025, 6},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(String) ->
    [Ops| ValuesS] = lists:reverse(tools:parse_lines(String, fun(L) -> string:tokens(L, " ") end)),
    Values = lists:map(fun (L) -> [list_to_integer(I)|| I <- L] end, ValuesS),

    G = tools:lists_to_grid(Values),
    Gr = tools:rotate_grid(G, cw),
    ValuesR = tools:grid_to_lists(Gr),

    solve_column(Ops, ValuesR, 0).

star2(String) ->
    [ValuesS, OpsS] = string:split(String, "\n", trailing),
    Grid = tools:parse_grid(ValuesS),
    GridR = tools:rotate_grid(Grid, ccw),
    % tools:print_grid(GridR),

    Ops = string:tokens(lists:reverse(OpsS)," "),
    Input = string:join(tools:grid_to_lists(GridR), "\n"),
    Values = tools:parse_blocks(string:replace(Input, " ", "", all), fun tools:parse_integers/1),

    solve_column(Ops, Values, 0).

read(File) ->
    tools:read_string(File).

solve_column([], [], Acc) ->
    Acc;
solve_column(["+"|Ops], [V|Values], Acc) ->
    solve_column(Ops, Values, Acc+ lists:sum(V));
solve_column(["*"|Ops], [V|Values], Acc) ->
    solve_column(Ops, Values, Acc+ tools:product(V)).
