-module(aoc2015_day14).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, star2/2, read/1]).

info() ->
    Examples = [
        {"examples/2015/day14_ex.txt", {star1, 1000}, 1120},
        {"examples/2015/day14_ex.txt", {star2, 1000}, 689}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 14},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(FlightData) ->
    star1(FlightData, 2503).

star1(FlightData, Time) ->
    lists:max([distance(Reindeer, Time) || Reindeer <- FlightData]).

star2(FlightData) ->
    star2(FlightData, 2503).

star2(FlightData, Time) ->
    Points = lists:flatten([distance2(Reindeer, 0, 0, Time) || Reindeer <- FlightData]),

    Best = keep_best(tools:reverse_sort(Points), []),
    Count = tools:count([R || {_, _, R} <- Best]),
    lists:max(maps:values(Count)).

read(File) ->
    tools:read_multiple_formats(
        File, "~s can fly ~d km/s for ~d seconds, but then must rest for ~d seconds."
    ).

distance([_Name, Speed, FlightTime, Rest], Time) ->
    FullIterations = Time div (FlightTime + Rest),
    RemainingTime = Time rem (FlightTime + Rest),
    FullIterations * Speed * FlightTime + min(FlightTime, RemainingTime) * Speed.

distance2(_Data, _Time, _Dist, 0) ->
    [];
distance2([Name, Speed, FlightTime, Rest] = Data, Time, Dist, TimeLeft) when TimeLeft > 0 ->
    MoveTime = min(FlightTime, TimeLeft),

    Moving = [{Time + T, Dist + Speed * T, Name} || T <- lists:seq(1, MoveTime)],
    RestTime = min(TimeLeft - MoveTime, Rest),

    Resting = [{Time + MoveTime + T, Dist + MoveTime * Speed, Name} || T <- lists:seq(1, RestTime)],
    Moving ++ Resting ++
        distance2(
            Data,
            Time + MoveTime + RestTime,
            Dist + MoveTime * Speed,
            TimeLeft - RestTime - MoveTime
        ).

keep_best([{A, _, _} = Best, {A, _, _} | Rest], Acc) ->
    keep_best([Best | Rest], Acc);
keep_best([Best | Rest], Acc) ->
    keep_best(Rest, [Best | Acc]);
keep_best([], Acc) ->
    Acc.
