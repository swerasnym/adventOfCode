-module(aoc2022_day4).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day4.txt"),
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

%% Note started 15 min late
read(File) ->
    tools:read_lines(File,
                     fun(L) ->
                        [[A, B, C, D]] = tools:parse_format(L, "~d-~d,~d-~d"),
                        {{A, B}, {C, D}}
                     end).

star1(Data) ->
    length(lists:filter(fun contained/1, Data)).

star2(Data) ->
    length(lists:filter(fun overlapps/1, Data)).

contained({{A, B}, {C, D}}) when A >= C, B =< D ->
    true;
contained({{A, B}, {C, D}}) when C >= A, D =< B ->
    true;
contained(_) ->
    false.

overlapps({{_, B}, {C, D}}) when C =< B, B =< D ->
    true;
overlapps({{A, _}, {C, D}}) when C =< A, A =< D ->
    true;
overlapps({{A, B}, {_, D}}) when A =< D, D =< B ->
    true;
overlapps({{A, B}, {C, _}}) when A =< C, C =< B ->
    true;
overlapps(_) ->
    false.
