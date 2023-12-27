-module(aoc2023_day3).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2023/day3_ex1.txt", star1, 4361},
        {"examples/2023/day3_ex1.txt", star2, 467835}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 3},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).
star1(Grid) ->
    Ns = find_numbers(Grid),
    lists:sum(Ns).

star2(Grid) ->
    lists:sum(find_gears(Grid)).

read(File) ->
    tools:read_grid(File, fun tokens/1).

tokens($.) ->
    empty;
tokens(D) when $0 =< D, $9 >= D ->
    D - $0;
tokens($*) ->
    {symbol, gear};
tokens(S) ->
    {symbol, S}.

neighbours({X0, Y0}) ->
    [{X0 + X, Y0 + Y} || X <- [-1, 0, 1], Y <- [-1, 0, 1], {X, Y} /= {0, 0}].

get_neighbours(Grid, Pos) ->
    [{P, V} || P <- neighbours(Pos), (V = maps:get(P, Grid, empty)) /= empty].

cmp({{X1, Y1}, _}, {{X2, Y2}, _}) ->
    {Y1, X1} =< {Y2, X2}.

find_numbers(Grid) ->
    Dn = [N || Pos := {symbol, _} <- Grid, N <- get_neighbours(Grid, Pos)],
    Ext = [L || {Pos, _} <- Dn, L <- extend(Pos, Grid)],
    Sorted = lists:usort(fun cmp/2, Dn ++ Ext),
    numbers(Sorted).

find_gears(Grid) ->
    PG = [get_neighbours(Grid, Pos) || Pos := {symbol, gear} <- Grid],
    EPG = [[L || {Pos, _} <- Dn, L <- extend(Pos, Grid)] ++ Dn || Dn <- PG],
    NPG = [numbers(lists:usort(fun cmp/2, E)) || E <- EPG],
    [A * B || [A, B] <- NPG].

extend(Pos, Grid) ->
    extend_left(Pos, Grid, []) ++ extend_right(Pos, Grid, []).

extend_left({X, Y}, Grid, Res) ->
    P = {X - 1, Y},

    case maps:get(P, Grid, empty) of
        empty ->
            Res;
        {symbol, _} ->
            Res;
        D ->
            extend_left(P, Grid, [{P, D} | Res])
    end.

extend_right({X, Y}, Grid, Res) ->
    P = {X + 1, Y},
    case maps:get(P, Grid, empty) of
        empty ->
            Res;
        {symbol, _} ->
            Res;
        D ->
            extend_right(P, Grid, [{P, D} | Res])
    end.

numbers([{Pos, D} | Rest]) ->
    numbers(Rest, Pos, D, []).

numbers([], _, Ds, Res) ->
    [Ds | Res];
numbers([{{X1, Y}, D} | Rest], {X0, Y}, Ds, Res) when X1 == X0 + 1 ->
    numbers(Rest, {X1, Y}, D + Ds * 10, Res);
numbers([{Pos, D} | Rest], _, Ds, Res) ->
    numbers(Rest, Pos, D, [Ds | Res]).
