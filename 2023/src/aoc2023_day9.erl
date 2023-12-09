-module(aoc2023_day9).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day9.txt"),
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

star1(Data) ->
    lists:sum([extrrapolate(D, fwd) || D <- Data]).

star2(Data) ->
    lists:sum([extrrapolate(D, rev) || D <- Data]).

read(File) ->
    tools:read_lines(File, parse_integers).

extrrapolate(List, Dir) ->
    case all_zero(List) of
        true ->
            0;
        false when Dir == fwd ->
            lists:last(List) + extrrapolate(dderiv(List, []), Dir);
        false when Dir == rev ->
            hd(List) - extrrapolate(dderiv(List, []), Dir)
    end.
dderiv([_], Res) ->
    lists:reverse(Res);
dderiv([A | [B | _] = Rest], Res) ->
    dderiv(Rest, [B - A | Res]).

all_zero([]) ->
    true;
all_zero([0 | Rest]) ->
    all_zero(Rest);
all_zero(_) ->
    false.
