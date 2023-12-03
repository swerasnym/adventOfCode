-module(aoc2021_day12).

-export([run/2, profile/3]).

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

profile(Star, File, Times) ->
    Data = read(File),

    case Star of
        star1 ->
            profile(fun() -> star1(Data) end, Times);
        star2 ->
            profile(fun() -> star2(Data) end, Times);
        _ ->
            Star1 = profile(fun() -> star1(Data) end, Times),
            Star2 = profile(fun() -> star2(Data) end, Times),
            {Star1, Star2}
    end.

profile(F, Times) ->
    Expected = F(),
    Results =
        [
            begin
                {Time, Expected} = timer:tc(F),
                Time
            end
         || _ <- lists:seq(1, Times)
        ],
    {Expected, lists:sum(Results) / Times / 1000}.

star1(Data) ->
    paths(start, [], Data, true).

star2(Data) ->
    paths(start, [], Data, false).

read(File) ->
    erase(),
    Lines = tools:read_lines(File),
    Paths =
        [
            begin
                [T, F] = string:split(Line, "-"),
                Ta = list_to_atom(T),
                Fa = list_to_atom(F),
                put(Fa, small(F)),
                put(Ta, small(T)),
                [Fa, Ta]
            end
         || Line <- Lines
        ],
    Rooms = lists:umerge(Paths),
    maps:from_list([neigbours(Room, Paths, []) || Room <- Rooms]).

neigbours(Room, [], Acc) ->
    {Room, lists:delete(start, Acc)};
neigbours(Room, [[Room, N] | Rest], Acc) ->
    neigbours(Room, Rest, [N | Acc]);
neigbours(Room, [[N, Room] | Rest], Acc) ->
    neigbours(Room, Rest, [N | Acc]);
neigbours(Room, [_ | Rest], Acc) ->
    neigbours(Room, Rest, Acc).

paths('end', _Visited, _Nerbours, _) ->
    1;
paths(Pos, Visited, Neigbours, true) ->
    case small_visited(Pos, Visited) of
        true ->
            0;
        false ->
            Visited1 = visit(Pos, Visited),
            lists:sum([paths(N, Visited1, Neigbours, true) || N <- maps:get(Pos, Neigbours)])
    end;
paths(Pos, Visited, Neigbours, false) ->
    Repeat = small_visited(Pos, Visited),
    Visited1 = visit(Pos, Visited),
    lists:sum([paths(N, Visited1, Neigbours, Repeat) || N <- maps:get(Pos, Neigbours)]).

small_visited(Pos, Visited) ->
    get(Pos) and lists:member(Pos, Visited).

small(Pos) when is_atom(Pos) ->
    get(Pos);
small(Pos) ->
    string:to_lower(Pos) == Pos.

visit(Pos, Visited) ->
    case get(Pos) of
        true ->
            [Pos | Visited];
        false ->
            Visited
    end.
