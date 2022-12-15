-module(aoc2022_day15).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day15.txt"),
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

-define(STAR1_Y, 2000000).

read(File) ->
    [tools:group(2, L)
     || L <- tools:read_format(File, "Sensor at x=~d, y=~d: closest beacon is at x=~d, y=~d")].

star1(Data) ->
    SensorDist = [{S, distm(S, B)} || [S, B] <- Data],
    PossibleEndpoints = lists:sort([no_beacons_at_y(SD, ?STAR1_Y) || SD <- SensorDist]),
    Endpoints = [E || E <- PossibleEndpoints, E /= none],
    sum_endpoints(Endpoints, 0).

star2(_Data) ->
    ok.

distm({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

no_beacons_at_y({S = {SX, SY}, D}, Y) ->
    case D - abs(SY - Y) of
        Delta when Delta >= 0 ->
            A = SX - Delta,
            B = SX + Delta,
            D = distm(S, {A, Y}),
            D = distm(S, {B, Y}),
            {A, B};
        _ ->
            none
    end.

sum_endpoints([], Acc) ->
    Acc;
sum_endpoints([{Min1, Max1}, {Min2, Max2} | Rest], Acc) when Min2 =< Max2 ->
    sum_endpoints([{Min1, max(Max1, Max2)} | Rest], Acc);
sum_endpoints([{Min, Max} | Rest], Acc) ->
    sum_endpoints(Rest, Acc + Max - Min).
