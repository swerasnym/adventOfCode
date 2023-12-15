-module(aoc2023_day04).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2023, 4}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

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
