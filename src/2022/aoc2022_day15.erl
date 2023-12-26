-module(aoc2022_day15).
-behaviour(aoc_solution).

-export([run/0, run/2, star2/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(
        aoc_solution:default_info(),
        #{
            problem => {2022, 15},
            all => [star1, star2, {star2, old}]
        }
    ).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

-define(STAR1_Y, 2000000).
-define(STAR2_Y, 4000000).

read(File) ->
    [
        tools:group(2, L)
     || L <- tools:read_format(File, "Sensor at x=~d, y=~d: closest beacon is at x=~d, y=~d")
    ].

star1(Data) ->
    SensorDist = [{S, distm(S, B)} || [S, B] <- Data],
    PossibleEndpoints = lists:sort([no_beacons_at_y(SD, ?STAR1_Y) || SD <- SensorDist]),
    Endpoints = [E || E <- PossibleEndpoints, E /= none],
    sum_endpoints(Endpoints, 0).

star2(Data, old) ->
    SensorDist = [{S, distm(S, B)} || [S, B] <- Data],
    {X, Y} = check_y(SensorDist, 0),
    X * ?STAR2_Y + Y.

star2(Data) ->
    SensorDist = [{S, distm(S, B)} || [S, B] <- Data],
    {PosXS, NegXS} = lists:unzip([lines(SD) || SD <- SensorDist]),
    PosX = lists:sort(lists:flatten(PosXS)),

    NegX = lists:sort(lists:flatten(NegXS)),

    Points =
        [
            {X, Y}
         || A <- repeated(PosX, []),
            B <- repeated(NegX, []),
            (X = (B - A) div 2) >= 0,
            (Y = (B + A) div 2) >= 0,
            X =< ?STAR2_Y,
            Y =< ?STAR2_Y
        ],

    [{X, Y}] = lists:filter(fun(P) -> test(SensorDist, P) end, Points),
    X * ?STAR2_Y + Y.

distm({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

no_beacons_at_y({{SX, SY}, D}, Y) ->
    case D - abs(SY - Y) of
        Delta when Delta >= 0 ->
            {SX - Delta, SX + Delta};
        _ ->
            none
    end.

sum_endpoints([], Acc) ->
    Acc;
sum_endpoints([{Min1, Max1}, {Min2, Max2} | Rest], Acc) when Min2 =< Max1 + 1 ->
    sum_endpoints([{Min1, max(Max1, Max2)} | Rest], Acc);
sum_endpoints([{Min, Max} | Rest], Acc) ->
    sum_endpoints(Rest, Acc + Max - Min).

group_endpoints([], Acc) ->
    lists:reverse(Acc);
group_endpoints([{Min1, Max1}, {Min2, Max2} | Rest], Acc) when Min2 =< Max1 + 1 ->
    group_endpoints([{Min1, max(Max1, Max2)} | Rest], Acc);
group_endpoints([{Min, Max} | Rest], Acc) ->
    group_endpoints(Rest, [{Min, Max} | Acc]).

check_y(_, Y) when Y > ?STAR2_Y ->
    error(no_pos_found);
check_y(SensorDist, Y) ->
    PossibleEndpoints = lists:sort([no_beacons_at_y(SD, Y) || SD <- SensorDist]),
    Endpoints = [{Min, Max} || {Min, Max} <- PossibleEndpoints, Max >= 0, Min =< ?STAR2_Y],
    Group = group_endpoints(Endpoints, []),
    case length(Group) of
        1 ->
            [{Min, Max}] = Group,
            case {Min =< 0, Max >= ?STAR2_Y} of
                {true, true} ->
                    check_y(SensorDist, Y + 1);
                {false, true} ->
                    {Min - 1, Y};
                {true, false} ->
                    {Max + 1, Y}
            end;
        2 ->
            [{_Min1, Max1}, {Min2, _Max2}] = Group,
            Max1 = Min2 - 2,
            {Max1 + 1, Y}
    end.

%% Find the k values of the lines lines y = +-x +k
%% of the lines distance 1 outside the sensor 'ranges'
lines({{X, Y}, D}) ->
    {[Y + D + 1 - X, Y - D - 1 - X], [Y + D + 1 + X, Y - D - 1 + X]}.

repeated([A, A | Rest], Acc) ->
    repeated(Rest, [A | Acc]);
repeated([_ | Rest], Acc) ->
    repeated(Rest, Acc);
repeated([], Acc) ->
    Acc.

test([], _) ->
    true;
test([{S, D} | Rest], P) ->
    case distm(S, P) > D of
        true ->
            test(Rest, P);
        false ->
            false
    end.
