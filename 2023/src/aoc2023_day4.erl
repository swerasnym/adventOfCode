-module(aoc2023_day4).

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

star1(Data) ->
    lists:sum([points(count_wins(Winning, Your)) || {_, Winning, Your} <- Data]).

star2(Data) ->
    Result = play2(Data, maps:from_keys(lists:seq(1, length(Data)), 1)),
    lists:sum(maps:values(Result)).

read(File) ->
    tools:read_lines(File, fun parse_card/1).

parse_card("Card " ++ Text) ->
    {ok, [Card], Rest} = io_lib:fread("~d: ", Text),
    [Winning, Your] = string:split(Rest, " | "),
    {Card, tools:parse_integers(Winning), tools:parse_integers(Your)}.

count_wins(Winning, Your) ->
    Ws = sets:from_list(Winning),
    Ys = sets:from_list(Your),
    sets:size(sets:intersection(Ws, Ys)).

points(0) -> 0;
points(N) -> tools:pow(2, N - 1).

play2([], Winnings) ->
    Winnings;
play2([{Card, WinNo, Your} | Rest], Winnings) ->
    Multiplyer = maps:get(Card, Winnings),
    Wins = count_wins(WinNo, Your),
    NewWin = #{C => maps:get(C, Winnings) + Multiplyer || C <- lists:seq(Card + 1, Card + Wins)},

    play2(Rest, maps:merge(Winnings, NewWin)).
