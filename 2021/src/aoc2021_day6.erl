-module(aoc2021_day6).

-export([run/2]).

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
        fun ({7, V}) ->
                {6, V + Zeros};
            ({K, V}) ->
                {K - 1, V}
        end,
    step(lists:map(Update, Rest) ++ [{8, Zeros}], Steps - 1).
