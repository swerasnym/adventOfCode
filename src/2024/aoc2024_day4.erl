-module(aoc2024_day4).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day4_ex.txt", star1, 18},
        {"examples/2024/day4_ex.txt", star2, 9}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 4},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Grid) ->
    length([W || Pos := $X <- Grid, W <- form_words(Pos, Grid), W == "XMAS"]).

star2(Grid) ->
    length([Pos || Pos := $A <- Grid, x_mas(form_word(directions2(Pos), Grid))]).

read(File) ->
    tools:read_grid(File).

directions({X, Y}) ->
    [
        [{X, Y}, {X + 1, Y}, {X + 2, Y}, {X + 3, Y}],
        [{X, Y}, {X - 1, Y}, {X - 2, Y}, {X - 3, Y}],
        [{X, Y}, {X, Y - 1}, {X, Y - 2}, {X, Y - 3}],
        [{X, Y}, {X, Y + 1}, {X, Y + 2}, {X, Y + 3}],
        [{X, Y}, {X + 1, Y + 1}, {X + 2, Y + 2}, {X + 3, Y + 3}],
        [{X, Y}, {X - 1, Y - 1}, {X - 2, Y - 2}, {X - 3, Y - 3}],
        [{X, Y}, {X + 1, Y - 1}, {X + 2, Y - 2}, {X + 3, Y - 3}],
        [{X, Y}, {X - 1, Y + 1}, {X - 2, Y + 2}, {X - 3, Y + 3}]
    ].

form_word(L, Grid) ->
    [maps:get(Pos, Grid, $.) || Pos <- L].

form_words(Pos, Grid) ->
    [form_word(L, Grid) || L <- directions(Pos)].

directions2({X, Y}) ->
    [{X - 1, Y - 1}, {X, Y}, {X + 1, Y + 1}, {X - 1, Y + 1}, {X, Y}, {X + 1, Y - 1}].

x_mas("MASMAS") ->
    true;
x_mas("MASSAM") ->
    true;
x_mas("SAMMAS") ->
    true;
x_mas("SAMSAM") ->
    true;
x_mas(_) ->
    false.
