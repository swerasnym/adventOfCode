-module(aoc2023_day1).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day1_ex2.txt"),
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
number([Head | _] = List) ->
    [H2 | _] = lists:reverse(List),
    erlang:list_to_integer([Head, H2]).

filter2([], V) ->
    lists:reverse(V);
filter2([N | Rest], V) when N >= $0, N =< $9 ->
    filter2(Rest, [N | V]);
filter2("one" ++ Rest, V) ->
    filter2("ne" ++ Rest, [$1 | V]);
filter2("two" ++ Rest, V) ->
    filter2("wo" ++ Rest, [$2 | V]);
filter2("three" ++ Rest, V) ->
    filter2("hree" ++ Rest, [$3 | V]);
filter2("four" ++ Rest, V) ->
    filter2("our" ++ Rest, [$4 | V]);
filter2("five" ++ Rest, V) ->
    filter2("ive" ++ Rest, [$5 | V]);
filter2("six" ++ Rest, V) ->
    filter2("ix" ++ Rest, [$6 | V]);
filter2("seven" ++ Rest, V) ->
    filter2("even" ++ Rest, [$7 | V]);
filter2("eight" ++ Rest, V) ->
    filter2("ight" ++ Rest, [$8 | V]);
filter2("nine" ++ Rest, V) ->
    filter2("ine" ++ Rest, [$9 | V]);
filter2([_ | Rest], V) ->
    filter2(Rest, V).
