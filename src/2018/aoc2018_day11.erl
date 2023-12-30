-module(aoc2018_day11).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, 18}, star1, "33,45"},
        {{data, 42}, star1, "21,61"}
        %{"examples/2018/day11_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 11},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(GridSerialNumber) ->
    GridMax = 300,
    PowerGrid = power_grid(GridSerialNumber, GridMax),
    PartialSums = partial_sums(PowerGrid),
    {_, {X, Y}} = Max = max_power(GridMax, PartialSums, 3),
    io:format("~p~n", [Max]),
    lists:flatten(io_lib:format("~p,~p", [X, Y])).

star2(GridSerialNumber) ->
    GridMax = 300,
    PowerGrid = power_grid(GridSerialNumber, GridMax),
    PartialSums = partial_sums(PowerGrid),
    Max = lists:max([{max_power(GridMax, PartialSums, S), S} || S <- lists:seq(1, GridMax)]),
    io:format("~p~n", [Max]),
    {{_, {X, Y}}, S} = Max,
    lists:flatten(io_lib:format("~p,~p,~p", [X, Y, S])).

read(File) ->
    erlang:list_to_integer(tools:read_string(File)).

power_level(X, Y, SN) ->
    RackId = X + 10,
    PL1 = RackId * Y + SN,
    PL2 = PL1 * RackId,
    PL3 = (PL2 div 100) rem 10,
    PL3 - 5.

power_grid(SN, Max) ->
    ok,
    #{{X, Y} => power_level(X, Y, SN) || X <- lists:seq(1, Max), Y <- lists:seq(1, Max)}.

partial_sums(Map) ->
    build_partial_sums(lists:sort(maps:keys(Map)), Map, #{}).

build_partial_sums([], _Map, PartialSums) ->
    PartialSums;
build_partial_sums([{X, Y} = P | Rest], Map, PS) ->
    Above = maps:get({X, Y - 1}, PS, 0),
    Left = maps:get({X - 1, Y}, PS, 0),
    Cell = maps:get(P, Map),
    Diag = maps:get({X - 1, Y - 1}, PS, 0),

    Sum = Above + Left + Cell - Diag,
    % io:format("~p: s: ~p a: ~p l: ~p c: ~p d: ~p~n", [P, Sum, Above, Left, Cell, Diag]),
    build_partial_sums(Rest, Map, PS#{P => Sum}).

total_power({X, Y} = P, PartialSums, SizeM1) ->
    Bottom = maps:get({X + SizeM1, Y + SizeM1}, PartialSums),
    Left = maps:get({X - 1, Y + SizeM1}, PartialSums, 0),
    Top = maps:get({X + SizeM1, Y - 1}, PartialSums, 0),
    Corner = maps:get({X - 1, Y - 1}, PartialSums, 0),

    {Bottom - Top - Left + Corner, P}.

max_power(GridMax, PartialSums, Size) ->
    TotalPowers = [
        total_power({X, Y}, PartialSums, Size - 1)
     || X <- lists:seq(1, GridMax - Size + 1), Y <- lists:seq(1, GridMax - Size + 1)
    ],
    lists:max(TotalPowers).
