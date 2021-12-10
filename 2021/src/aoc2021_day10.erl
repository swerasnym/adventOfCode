-module(aoc2021_day10).

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
    Results = [check(D, []) || D <- Data],
    lists:sum([V || {error, V} <- Results]).

star2(Data) ->
    Results = [check(D, []) || D <- Data],
    Incoplete = [I || I <- Results, is_list(I)],
    Scores =
        lists:sort([lists:foldl(fun(V, Score) -> Score * 5 + score_i(V) end, 0, Line)
                    || Line <- Incoplete]),

    lists:nth(length(Scores) div 2 + 1, Scores).

read(File) ->
    tools:read_lines(File).

check([C | Rest], [C | RestToClose]) ->
    check(Rest, RestToClose);
check([C | Rest], RestToClose) ->
    case close(C) of
        false ->
            {error, score(C)};
        CC ->
            check(Rest, [CC | RestToClose])
    end;
check([], Left) ->
    Left.

close(${) ->
    $};
close($() ->
    $);
close($[) ->
    $];
close($<) ->
    $>;
close(_) ->
    false.

score($}) ->
    1197;
score($)) ->
    3;
score($]) ->
    57;
score($>) ->
    25137.

score_i($}) ->
    3;
score_i($)) ->
    1;
score_i($]) ->
    2;
score_i($>) ->
    4.
