-module(aoc2019_day1).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 1},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Fuel = lists:map(fun fuel/1, Data),
    lists:sum(Fuel).

star2(Data) ->
    Fuel = lists:map(fun acc_fuel/1, Data),
    lists:sum(Fuel).

read(File) ->
    tools:read_integers(File).

acc_fuel(Weight) ->
    acc_fuel(Weight, 0).

acc_fuel(Weight, Acc) ->
    case fuel(Weight) of
        Fuel when Fuel > 0 ->
            acc_fuel(Fuel, Acc + Fuel);
        _ ->
            Acc
    end.

fuel(Weight) ->
    Weight div 3 - 2.
