-module(day11).

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

star1(Map) ->
    tools:count(occupied, iterate1(Map)).

star2(Map) ->
    tools:count(occupied, iterate2(Map)).

read(File) ->
    maps:without([max],
                 tools:read_grid(File,
                                 #{$# => occupied,
                                   $. => floor,
                                   $L => free})).

update_seat1(_Seat, floor, _OldMap, NewMap) ->
    NewMap;
update_seat1(Seat, Value, OldMap, NewMap) ->
    case {Value, tools:count(occupied, neigbours1(Seat, OldMap))} of
        {free, 0} ->
            NewMap#{Seat => occupied};
        {occupied, N} when N >= 4 ->
            NewMap#{Seat => free};
        _ ->
            NewMap
    end.

neigbours1({X, Y}, Map) ->
    [maps:get({Xn, Yn}, Map, wall)
     || Xn <- lists:seq(X - 1, X + 1), Yn <- lists:seq(Y - 1, Y + 1), {Xn, Yn} /= {X, Y}].

iterate1(Map) ->
    F = fun(Seat, Value, Acc) -> update_seat1(Seat, Value, Map, Acc) end,
    case maps:fold(F, Map, Map) of
        Map ->
            Map;
        New ->
            iterate1(New)
    end.

update_seat2(_Seat, floor, _OldMap, NewMap) ->
    NewMap;
update_seat2(Seat, Value, OldMap, NewMap) ->
    case {Value, tools:count(occupied, neigbours2(Seat, OldMap))} of
        {free, 0} ->
            NewMap#{Seat => occupied};
        {occupied, N} when N >= 5 ->
            NewMap#{Seat => free};
        _ ->
            NewMap
    end.

neigbours2(Seat, Map) ->
    [find_seat(Seat, {Dx, Dy}, Map)
     || Dx <- lists:seq(-1, 1), Dy <- lists:seq(-1, 1), {Dx, Dy} /= {0, 0}].

find_seat({X, Y}, {Dx, Dy} = Dir, Map) ->
    case maps:get({X + Dx, Y + Dy}, Map, wall) of
        wall ->
            wall;
        floor ->
            find_seat({X + Dx, Y + Dy}, Dir, Map);
        Value ->
            Value
    end.

iterate2(Map) ->
    F = fun(Seat, Value, Acc) -> update_seat2(Seat, Value, Map, Acc) end,
    case maps:fold(F, Map, Map) of
        Map ->
            Map;
        New ->
            iterate2(New)
    end.
