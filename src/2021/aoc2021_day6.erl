-module(aoc2021_day6).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 6},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Simulate = step(Data, 80),
    {_, Counts} = lists:unzip(Simulate),
    lists:sum(Counts).

star2(Data) ->
    Simulate = step(Data, 256),
    {_, Counts} = lists:unzip(Simulate),
    lists:sum(Counts).

read(File) ->
    Fishes = tools:read_integers(File, ","),
    [{D, tools:count(D, Fishes)} || D <- lists:seq(0, 8)].

step(Result, 0) ->
    Result;
step([{0, Zeros} | Rest], Steps) ->
    Update =
        fun
            ({7, V}) ->
                {6, V + Zeros};
            ({K, V}) ->
                {K - 1, V}
        end,
    step(lists:map(Update, Rest) ++ [{8, Zeros}], Steps - 1).
