-module(aoc2023_day13).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2023, 13}}).

run() ->
    %% aoc_solution:run(?MODULE, all, "2023/data/day13_ex.txt").
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Grids) ->
    lists:sum([value_ref(find_reflections(G)) || G <- Grids]).

star2(Grids) ->
    lists:sum([value_smudge(smudge(G), find_reflections(G)) || G <- Grids]).

read(File) ->
    tools:read_blocks(File, parse_grid).

value_smudge(S, {A, B}) ->
    lists:max([value_ref({Sa -- A, Sb -- B}) || {Sa, Sb} <- S]).

value_ref({[], []}) ->
    0;
value_ref({[V], []}) ->
    V;
value_ref({[], [H]}) ->
    H * 100.

smudge(G) ->
    Rs = [find_reflections(smudge(Pos, G)) || Pos = {_, _} <- maps:keys(G)],
    [R || R <- Rs, R /= {[], []}].

smudge(Pos, G) ->
    case maps:get(Pos, G) of
        $. ->
            G#{Pos := $#};
        $# ->
            G#{Pos := $.}
    end.

find_reflections(#{} = GridH) ->
    Horisontal = find_reflections(tools:grid_to_lists(GridH)),
    GridV = tools:rotate_grid(GridH, cw),
    Vertival = find_reflections(tools:grid_to_lists(GridV)),
    {Vertival, Horisontal};
find_reflections([H | A]) ->
    find_reflections([H], A, 1, []).

find_reflections(_, [], _N, Res) ->
    Res;
find_reflections(A, [H | Br] = B, N, Res) ->
    ResN =
        case is_reflection(A, B) of
            true ->
                [N | Res];
            false ->
                Res
        end,
    find_reflections([H | A], Br, N + 1, ResN).

is_reflection(_, []) ->
    true;
is_reflection([], _) ->
    true;
is_reflection([H | A], [H | B]) ->
    is_reflection(A, B);
is_reflection(_, _) ->
    false.
