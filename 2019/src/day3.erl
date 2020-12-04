-module(day3).

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

star1({Data1, Data2}) ->
    S1 = sets:from_list(move(Data1)),
    S2 = sets:from_list(move(Data2)),
    Crossings =
        sets:to_list(
            sets:intersection(S1, S2)),
    Distanses = lists:map(fun distance/1, Crossings),
    lists:min(Distanses).

star2({Data1, Data2}) ->
    M1 = move(Data1),
    M2 = move(Data2),
    S1 = sets:from_list(M1),
    S2 = sets:from_list(M2),
    Crossings =
        sets:to_list(
            sets:intersection(S1, S2)),
    Z1 = lists:zip(M1, lists:seq(1, length(M1))),
    Z2 = lists:zip(M2, lists:seq(1, length(M2))),

    Distanses = lists:map(fun(Pos) -> distanse(Pos, Z1, Z2) end, Crossings),

    lists:min(Distanses).

read(File) ->
    {ok, Device} = file:open(File, [read]),
    Row1 = io:get_line(Device, ""),
    Row2 = io:get_line(Device, ""),

    List1 =
        string:split(
            string:trim(Row1), ",", all),
    List2 =
        string:split(
            string:trim(Row2), ",", all),

    Data1 = lists:map(fun parse/1, List1),
    Data2 = lists:map(fun parse/1, List2),

    {Data1, Data2}.

parse([C | IntStr]) ->
    {I, []} = string:to_integer(IntStr),
    {C, I}.

move([L | Ls]) ->
    move(L, {0, 0}, Ls, []).

move({_, 0}, _, [], Acc) ->
    lists:reverse(Acc);
move({_, 0}, Pos, [E | Es], Acc) ->
    move(E, Pos, Es, Acc);
move({$R, I}, {X, Y}, List, Acc) ->
    Pos = {X, Y + 1},
    move({$R, I - 1}, Pos, List, [Pos | Acc]);
move({$L, I}, {X, Y}, List, Acc) ->
    Pos = {X, Y - 1},
    move({$L, I - 1}, Pos, List, [Pos | Acc]);
move({$U, I}, {X, Y}, List, Acc) ->
    Pos = {X + 1, Y},
    move({$U, I - 1}, Pos, List, [Pos | Acc]);
move({$D, I}, {X, Y}, List, Acc) ->
    Pos = {X - 1, Y},
    move({$D, I - 1}, Pos, List, [Pos | Acc]).

distance({X, Y}) ->
    abs(X) + abs(Y).

distanse(Pos, L1, L2) ->
    {Pos, D1} = lists:keyfind(Pos, 1, L1),
    {Pos, D2} = lists:keyfind(Pos, 1, L2),
    D1 + D2.
