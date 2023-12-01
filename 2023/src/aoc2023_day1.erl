-module(aoc2023_day1).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day1.txt"),
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
    lists:sum([number(filter1(V, [])) || V <- Data]).

star2(Data) ->
    lists:sum([number(filter2(V, [])) || V <- Data]).

read(File) ->
    tools:read_lines(File).

filter1([], V) ->
    lists:reverse(V);
filter1([N | Rest], V) when N >= $0, N =< $9 ->
    filter1(Rest, [N | V]);
filter1([_ | Rest], V) ->
    filter1(Rest, V).

number([]) ->
    0;
number(D) ->
    list_to_integer([hd(D), lists:last(D)]).

filter2([], V) ->
    lists:reverse(V);
filter2([N | Rest], V) when N >= $0, N =< $9 ->
    filter2(Rest, [N | V]);
filter2([_ | Rest] = L, V) ->
    Numbers =
        lists:enumerate($1,
                        ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]),
    Match = [D || {D, Text} <- Numbers, tools:is_prefix(Text, L)],
    filter2(Rest, Match ++ V).
