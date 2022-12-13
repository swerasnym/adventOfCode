-module(aoc2022_day13).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day13.txt"),
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
    tools:read_blocks(File, parse_lines).

star1(Data) ->
    Comparasons = [compare(D) || D <- Data],
    lists:sum([I || {I, true} <- lists:enumerate(Comparasons)]).

star2(Data) ->
    Packets = ["[[2]]", "[[6]]"] ++ lists:merge(Data),
    Sorted = lists:sort(fun(A, B) -> compare([A, B]) end, Packets),
    % [io:format("~s~n", [L]) || L <- Sorted],
    tools:product([I || {I, V} <- lists:enumerate(Sorted), V == "[[2]]" orelse V == "[[6]]"]).

compare([A, B]) ->
    compare(tools:as_term(A), tools:as_term(B)).

compare(A, A) ->
    equal;
compare([A | RestA], [A | RestB]) ->
    compare(RestA, RestB);
compare([A | _RestA], [B | _RestB]) when is_integer(A), is_integer(B) ->
    A < B;
compare([A | RestA], [B | RestB]) when is_integer(A), is_list(B) ->
    compare([[A] | RestA], [B | RestB]);
compare([A | RestA], [B | RestB]) when is_integer(B), is_list(A) ->
    compare([A | RestA], [[B] | RestB]);
compare([], [_B | _]) ->
    true;
compare([_A | _], []) ->
    false;
compare([A | RestA], [B | RestB]) when is_list(A), is_list(B) ->
    case compare(A, B) of
        equal ->
            compare(RestA, RestB);
        Res ->
            Res
    end.
