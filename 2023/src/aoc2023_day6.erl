-module(aoc2023_day6).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day6.txt"),
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

star1({Time, Distance}) ->
    Data = lists:zip(tools:parse_integers(Time), tools:parse_integers(Distance)),
    tools:product([record(D) || D <- Data]).

star2({TimeS, DistanceS}) ->
    Time = erlang:list_to_integer(lists:flatten(string:replace(TimeS, " ", "", all))),
    Distance = erlang:list_to_integer(lists:flatten(string:replace(DistanceS, " ", "", all))),
    Min = bmin({0, Time div 2}, {Time, Distance}),
    Max = bmax({Time div 2, Time}, {Time, Distance}),
    Max - Min + 1.

read(File) ->
    ["Time:" ++ Time, "Distance:" ++ Distance] = tools:read_lines(File),
    {Time, Distance}.

dist(T, Max) ->
    T * (Max - T).

record({Time, MaxD}) ->
    length([T || T <- lists:seq(0, Time), dist(T, Time) > MaxD]).

bmin({Tmin, Tmax}, {Max, Record}) when Tmax - Tmin < 5 ->
    lists:min([T || T <- lists:seq(Tmin, Tmax), dist(T, Max) > Record]);
bmin({Tmin, Tmax}, {Max, Record} = R) ->
    T = (Tmin + Tmax + 1) div 2,
    case dist(T, Max) > Record of
        true ->
            bmin({Tmin, T - 1}, R);
        false ->
            bmin({T + 1, Tmax}, R)
    end.

bmax({Tmin, Tmax}, {Max, Record}) when Tmax - Tmin < 5 ->
    lists:max([T || T <- lists:seq(Tmin, Tmax), dist(T, Max) > Record]);
bmax({Tmin, Tmax}, {Max, Record} = R) ->
    T = (Tmin + Tmax + 1) div 2,
    case dist(T, Max) > Record of
        false ->
            bmax({Tmin, T - 1}, R);
        true ->
            bmax({T + 1, Tmax}, R)
    end.
