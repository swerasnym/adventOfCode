-module(aoc2021_day12).

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
    length(paths("start", [], Data)).

star2(Data) ->
    paths2("start", [], Data, false).

read(File) ->
    Lines = tools:read_lines(File),
    Paths = [string:split(Line, "-") || Line <- Lines],
    Rooms = lists:umerge(Paths),
    maps:from_list([neigbours(Room, Paths, []) || Room <- Rooms]).

neigbours(Room, [], Acc) ->
    {Room, lists:delete("start", Acc)};
neigbours(Room, [[Room, N] | Rest], Acc) ->
    neigbours(Room, Rest, [N | Acc]);
neigbours(Room, [[N, Room] | Rest], Acc) ->
    neigbours(Room, Rest, [N | Acc]);
neigbours(Room, [_ | Rest], Acc) ->
    neigbours(Room, Rest, Acc).

paths("end", Visited, _Nerbours) ->
    {Visited ++ ["end"]};
paths(Pos, Visited, Neigbours) ->
    case small_visited(Pos, Visited) of
        true ->
            [];
        false ->
            Visited1 = Visited ++ [Pos],
            lists:flatten([paths(N, Visited1, Neigbours) || N <- maps:get(Pos, Neigbours)])
    end.

paths2("end", _Visited, _Nerbours, _) ->
    1;
paths2(Pos, Visited, Neigbours, true) ->
    case small_visited(Pos, Visited) of
        true ->
            0;
        false ->
            Visited1 = Visited ++ [Pos],
            lists:sum([paths2(N, Visited1, Neigbours, true) || N <- maps:get(Pos, Neigbours)])
    end;
paths2(Pos, Visited, Neigbours, false) ->
    Repeat = small_visited(Pos, Visited),
    Visited1 = Visited ++ [Pos],
    lists:sum([paths2(N, Visited1, Neigbours, Repeat) || N <- maps:get(Pos, Neigbours)]).

small_visited(Pos, Visited) ->
    small(Pos) and lists:member(Pos, Visited).

small(Pos) ->
    string:to_lower(Pos) == Pos.
