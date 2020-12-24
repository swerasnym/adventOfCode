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
    New = iterate(Map),
    maps:get(occupied, count(maps:values(New))).

star2(Map) ->
    New = iterate2(Map),
    maps:get(occupied, count(maps:values(New))).

read(File) ->
    {ok, Bin} = file:read_file(File),
    List = binary_to_list(Bin),
    read(List, 0, 0, #{}).

read([$\n], _X, _Y, Acc) ->
    Acc; %% #{dim => {X, Y + 1}};
read([$\n | Rest], _X, Y, Acc) ->
    read(Rest, 0, Y + 1, Acc);
read([Char | Rest], X, Y, Acc) ->
    case Char of
        $# ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => occupied});
        $. ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => floor});
        $L ->
            read(Rest, X + 1, Y, Acc#{{X, Y} => free})
    end.

count(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end,
                #{occupied => 0},
                List).

update_seat(_Seat, floor, _OldMap, NewMap) ->
    NewMap;
update_seat(Seat, Value, OldMap, NewMap) ->
    case {Value, count(neigbours(Seat, OldMap))} of
        {free, #{occupied := 0}} ->
            NewMap#{Seat => occupied};
        {occupied, #{occupied := N}} when N >= 4 ->
            NewMap#{Seat => free};
        _ ->
            NewMap
    end.

neigbours({X, Y}, Map) ->
    [maps:get({X - 1, Y - 1}, Map, wall),
     maps:get({X - 1, Y}, Map, wall),
     maps:get({X - 1, Y + 1}, Map, wall),
     maps:get({X, Y - 1}, Map, wall),
     maps:get({X, Y + 1}, Map, wall),
     maps:get({X + 1, Y - 1}, Map, wall),
     maps:get({X + 1, Y}, Map, wall),
     maps:get({X + 1, Y + 1}, Map, wall)].

iterate(Map) ->
    F = fun(Seat, Value, Acc) -> update_seat(Seat, Value, Map, Acc) end,
    case maps:fold(F, Map, Map) of
        Map ->
            Map;
        New ->
            iterate(New)
    end.

update_seat2(_Seat, floor, _OldMap, NewMap) ->
    NewMap;
update_seat2(Seat, Value, OldMap, NewMap) ->
    case {Value, count(neigbours2(Seat, OldMap))} of
        {free, #{occupied := 0}} ->
            NewMap#{Seat => occupied};
        {occupied, #{occupied := N}} when N >= 5 ->
            NewMap#{Seat => free};
        _ ->
            NewMap
    end.

neigbours2(Seat, Map) ->
    [find(Seat, {-1, -1}, Map),
     find(Seat, {-1, 0}, Map),
     find(Seat, {-1, +1}, Map),
     find(Seat, {0, -1}, Map),
     find(Seat, {0, +1}, Map),
     find(Seat, {+1, -1}, Map),
     find(Seat, {+1, 0}, Map),
     find(Seat, {+1, +1}, Map)].

find({X, Y}, {Dx, Dy} = Dir, Map) ->
    case maps:get({X + Dx, Y + Dy}, Map, wall) of
        wall ->
            wall;
        floor ->
            find({X + Dx, Y + Dy}, Dir, Map);
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