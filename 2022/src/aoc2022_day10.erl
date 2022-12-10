-module(aoc2022_day10).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day10.txt"),
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

read(File) ->
    tools:read_lines(File).

star1(Data) ->
    {ValeLists, _} = lists:mapfoldl(fun runi/2, 1, Data),
    Values = [1] ++ lists:flatten(ValeLists),
    Strengths = [V * I || {I, V} <- lists:enumerate(Values), I rem 40 == 20, I =< 220],
    lists:sum(Strengths).

star2(Data) ->
    {ValeLists, _} = lists:mapfoldl(fun runi/2, 1, Data),
    Values = [1] ++ lists:flatten(ValeLists),
    Display = [draw(E) || E <- lists:enumerate(0, Values)],
    tools:print_grid(
        maps:from_list(Display)).

runi("noop", X) ->
    {[X], X};
runi("addx " ++ N, X) ->
    [[V]] = tools:parse_format(N, "~d"),
    {[X, X + V], X + V}.

draw({I, V}) ->
    P = I rem 40,
    R = I div 40,

    Symbol =
        case V - P of
            -1 ->
                $#;
            0 ->
                $#;
            1 ->
                $#;
            _ ->
                $
        end,
    {{P, R}, Symbol}.
