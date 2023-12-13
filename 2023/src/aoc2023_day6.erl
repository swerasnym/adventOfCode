-module(aoc2023_day6).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2023, 6}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).
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
