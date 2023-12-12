-module(aoc2023_day12).

-export([run/0, run/2]).

run() ->
    % {S1, S2} = Res = run(all, "/home/rasmus/repos/adventOfCode/2023/data/day12_ex.txt"),
    {S1, S2} = Res = run(all, aoc_web:get_input_path(2023, 12)),
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
    % [arrangements(Springs, Cond)|| {Springs, Cond} <-  Data].
    lists:sum([arr(Springs, Cond) || {Springs, Cond} <- Data]).

star2(Data) ->
    %[unfold(Springs, Cond)|| {Springs, Cond} <-  Data].
    lists:sum([unfold(Springs, Cond) || {Springs, Cond} <- Data]).

read(File) ->
    tools:read_lines(File, fun split_line/1).

split_line(Line) ->
    [Springs, Cond] = string:split(Line, " "),
    {lists:map(fun type/1, Springs), tools:parse_integers(Cond, ",")}.

type($.) -> operational;
type($#) -> damaged;
type($?) -> unknown.

unfold(Springs, Cond) ->
    arr(
        lists:flatten(lists:duplicate(4, Springs ++ [unknown]), Springs),
        lists:flatten(lists:duplicate(5, Cond))
    ).

arr(A, B) ->
    erlang:erase(),
    arrangements(A, B).

arrangements([], []) ->
    1;
arrangements([], [{0}]) ->
    1;
arrangements([], _) ->
    0;
arrangements([State | RestS], []) ->
    case State of
        operational ->
            arrangements(RestS, []);
        damaged ->
            0;
        unknown ->
            arrangements(RestS, [])
    end;
arrangements([State | RestS], [{0} | RestC]) ->
    case State of
        operational ->
            arrangements(RestS, RestC);
        damaged ->
            0;
        unknown ->
            arrangements(RestS, RestC)
    end;
arrangements([State | RestS], [{N} | RestC]) when N > 0 ->
    case State of
        operational ->
            0;
        damaged ->
            arrangements(RestS, [{N - 1} | RestC]);
        unknown ->
            arrangements(RestS, [{N - 1} | RestC])
    end;
arrangements([State | RestS] = A, [N | RestC] = B) when N > 0 ->
    Key = {length(A), length(B)},
    Res =
        case {erlang:get(Key), State} of
            {undefined, operational} ->
                arrangements(RestS, [N | RestC]);
            {undefined, damaged} ->
                arrangements(RestS, [{N - 1} | RestC]);
            {undefined, unknown} ->
                arrangements(RestS, [N | RestC]) + arrangements(RestS, [{N - 1} | RestC]);
            {Value, _} ->
                Value
        end,
    erlang:put(Key, Res),
    Res.
