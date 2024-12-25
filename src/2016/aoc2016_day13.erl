-module(aoc2016_day13).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

info() ->
    Examples = [
        {{data, 10}, {star1, {7, 4}}, 11}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 13},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Number) ->
    star1(Number, {31, 39}).

star1(Number, End) ->
    io:format("~p ~p~n", [Number, End]),
    GridO = #{{X, Y} => $. || X <- lists:seq(0, 9), Y <- lists:seq(0, 9), open({X, Y}, Number)},
    GridW = #{{X, Y} => $# || X <- lists:seq(0, 9), Y <- lists:seq(0, 9), not open({X, Y}, Number)},
    tools:print_grid(maps:merge(GridO, GridW)),

    {Steps, _, _} = aoc_graph:bfs({1, 1}, End, neighbours(Number)),
    Steps.

star2(Number) ->
    {no_path, Visited} = aoc_graph:bfs({0, {1, 1}}, all, neighbours2(Number)),
    Positions = lists:usort([P || {_, P} <- maps:keys(Visited)]),
    length(Positions).

read(File) ->
    erlang:list_to_integer(tools:read_string(File)).

open({X, _}, _) when X < 0 ->
    false;
open({_, Y}, _) when Y < 0 ->
    false;
open({X, Y}, Number) ->
    Bits = erlang:integer_to_list(X * X + 3 * X + 2 * X * Y + Y + Y * Y + Number, 2),
    (tools:count($1, Bits) rem 2) == 0.

neighbours(Number) ->
    fun(Pos) ->
        Dn = [aoc_vector:add(Pos, Dp) || Dp <- [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]],
        [D || D <- Dn, open(D, Number)]
    end.

neighbours2(Number) ->
    N = neighbours(Number),
    fun
        ({50, _}) ->
            [];
        ({S, Pos}) ->
            [{S + 1, P} || P <- N(Pos)]
    end.
