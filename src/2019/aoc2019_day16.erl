-module(aoc2019_day16).

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
    Result = phases(100, Data),
    {First, _} = lists:split(8, Result),

    list_to_integer([C + $0 || C <- First]).

star2(Data) ->
    {First, _} = lists:split(7, Data),
    Offset = list_to_integer([C + $0 || C <- First]),

    {_, List} = split(Offset, lists:flatten(lists:duplicate(10000, Data))),
    Result = phases2(100, List),

    {First2, _} = lists:split(8, Result),

    list_to_integer([C + $0 || C <- First2]).

read(File) ->
    {ok, Bin} = file:read_file(File),
    String = binary_to_list(string:trim(Bin)),
    [C - $0 || C <- String].

phases(0, List) ->
    List;
phases(N, List) ->
    phases(N - 1, phase2(List)).

phases2(0, List) ->
    List;
phases2(N, List) ->
    phases2(N - 1, phase3(List)).

phase(List) ->
    [phase(List, Position) || Position <- lists:seq(1, length(List))].

phase(List, Position) ->
    Didgit =
        lists:sum(lists:zipwith(fun(A, B) -> A * B end, List, pattern(Position, length(List)))),
    abs(Didgit) rem 10.

phase2(List) ->
    [phase2(List, Position) || Position <- lists:seq(1, length(List))].

phase2(List, Position) ->
    % drop first
    {_First1, Rest1} = split(Position - 1, List),
    {First2, Rest2} = split(Position, Rest1),
    {_First3, Rest3} = split(Position, Rest2),
    {First4, Rest4} = split(Position, Rest3),
    phase2(Rest4, Position, lists:sum(First2) - lists:sum(First4)).

phase2([], _Position, Acc) ->
    abs(Acc) rem 10;
phase2(List, Position, Acc) ->
    {_First1, Rest1} = split(Position, List),
    {First2, Rest2} = split(Position, Rest1),
    {_First3, Rest3} = split(Position, Rest2),
    {First4, Rest4} = split(Position, Rest3),
    phase2(Rest4, Position, Acc + lists:sum(First2) - lists:sum(First4)).

pattern(Position, Length) ->
    Base = [0, 1, 0, -1],
    Rep = lists:flatten([lists:duplicate(Position, D) || D <- Base]),

    case length(Rep) > Length of
        true ->
            [_ | Rest] = Rep,
            {Pattern, _} = lists:split(Length, Rest),
            Pattern;
        false ->
            Repititions = Length div length(Rep) + 1,

            [_ | Rest] = lists:flatten(lists:duplicate(Repititions, Rep)),
            {Pattern, _} = lists:split(Length, Rest),
            Pattern
    end.

split(N, List) ->
    case N > length(List) of
        true ->
            {List, []};
        false ->
            lists:split(N, List)
    end.

phase3(List) ->
    Reversed = lists:reverse(List),
    calculate(Reversed, 0, []).

calculate([], Prev, Acc) ->
    Acc;
calculate([N | Ns], Prev, Acc) ->
    Value = abs(N + Prev) rem 10,
    calculate(Ns, Value, [Value | Acc]).
