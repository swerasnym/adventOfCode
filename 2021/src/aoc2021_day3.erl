-module(aoc2021_day3).

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
    Counts = count_bits(Data),
    G = list_to_integer(gamma(Counts, []), 2),
    E = list_to_integer(epsilon(Counts, []), 2),
    E * G.

star2(Data) ->
    Oxy = oxygen(Data, 1),
    CO2 = co2(Data, 1),
    list_to_integer(Oxy, 2) * list_to_integer(CO2, 2).

read(File) ->
    tools:read_lines(File).

count_bits(Values) ->
    Length = length(hd(Values)),
    [count_bits(Pos, Values, {0, 0}) || Pos <- lists:seq(1, Length)].

count_bits(Pos, [Val | Values], {Zeros, Ones}) ->
    case lists:nth(Pos, Val) of
        $0 ->
            count_bits(Pos, Values, {Zeros + 1, Ones});
        $1 ->
            count_bits(Pos, Values, {Zeros, Ones + 1})
    end;
count_bits(_Pos, [], Result) ->
    Result.

gamma([{Zeros, Ones} | Counts], Res) ->
    case Zeros > Ones of
        true ->
            gamma(Counts, [$0 | Res]);
        false ->
            gamma(Counts, [$1 | Res])
    end;
gamma([], Res) ->
    lists:reverse(Res).

epsilon([{Zeros, Ones} | Counts], Res) ->
    case Zeros > Ones of
        true ->
            epsilon(Counts, [$1 | Res]);
        false ->
            epsilon(Counts, [$0 | Res])
    end;
epsilon([], Res) ->
    lists:reverse(Res).

oxygen([Result], _Pos) ->
    Result;
oxygen(Values, Pos) ->
    {Zeros, Ones} = count_bits(Pos, Values, {0, 0}),
    Filter =
        case Zeros > Ones of
            true ->
                $0;
            false ->
                $1
        end,

    Left = lists:filter(fun(Elem) -> Filter == lists:nth(Pos, Elem) end, Values),
    oxygen(Left, Pos + 1).

co2([Result], _Pos) ->
    Result;
co2(Values, Pos) ->
    {Zeros, Ones} = count_bits(Pos, Values, {0, 0}),
    Filter =
        case Zeros =< Ones of
            true ->
                $0;
            false ->
                $1
        end,

    Left = lists:filter(fun(Elem) -> Filter == lists:nth(Pos, Elem) end, Values),
    co2(Left, Pos + 1).
