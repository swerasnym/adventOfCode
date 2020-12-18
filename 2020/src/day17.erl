-module(day17).

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
    Empty =
        maps:from_list([{{X, Y, Z, 0}, inactive}
                        || Z <- lists:seq(-6, 6), Y <- lists:seq(-8, 16), X <- lists:seq(-8, 16)]),
    #{active := Result} = iterate3d(maps:merge(Empty, Data), 6),
    Result.

star2(Data) ->
    Empty =
        maps:from_list([{{X, Y, Z, W}, inactive}
                        || W <- lists:seq(-6, 6),
                           Z <- lists:seq(-6, 6),
                           Y <- lists:seq(-6, 13),
                           X <- lists:seq(-6, 13)]),
    #{active := Result} = iterate4d(maps:merge(Empty, Data), 6),
    Result.

iterate3d(Map, 0) ->
    count(maps:values(Map));
iterate3d(Map, N) ->
    F = fun(Pos, Value, Acc) -> update3d(Pos, Value, Map, Acc) end,
    iterate3d(maps:fold(F, Map, Map), N - 1).

iterate4d(Map, 0) ->
    count(maps:values(Map));
iterate4d(Map, N) ->
    F = fun(Pos, Value, Acc) -> update4d(Pos, Value, Map, Acc) end,
    iterate4d(maps:fold(F, Map, Map), N - 1).

count(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end,
                #{active => 0},
                List).

update3d(Pos, Value, OldMap, NewMap) ->
    case {Value, count(neigbours3d(Pos, OldMap))} of
        {active, #{active := N}} when N < 2; N > 3 ->
            NewMap#{Pos => inactive};
        {inactive, #{active := 3}} ->
            NewMap#{Pos => active};
        _ ->
            NewMap
    end.

update4d(Pos, Value, OldMap, NewMap) ->
    case {Value, count(neigbours4d(Pos, OldMap))} of
        {active, #{active := N}} when N < 2; N > 3 ->
            NewMap#{Pos => inactive};
        {inactive, #{active := 3}} ->
            NewMap#{Pos => active};
        _ ->
            NewMap
    end.

read(File) ->
    {ok, Bin} = file:read_file(File),
    List = binary_to_list(Bin),
    read(List, 0, 0, #{}).

read([$\n], _X, _Y, Acc) ->
    Acc;
read([$\n | Rest], _X, Y, Acc) ->
    read(Rest, 0, Y + 1, Acc);
read([Char | Rest], X, Y, Acc) ->
    case Char of
        $# ->
            read(Rest, X + 1, Y, Acc#{{X, Y, 0, 0} => active});
        $. ->
            read(Rest, X + 1, Y, Acc#{{X, Y, 0, 0} => inactive})
    end.

neigbours3d({X, Y, Z, _W}, Map) ->
    [maps:get({Xn, Yn, Zn, 0}, Map, wall)
     || Xn <- lists:seq(X - 1, X + 1),
        Yn <- lists:seq(Y - 1, Y + 1),
        Zn <- lists:seq(Z - 1, Z + 1),
        {Xn, Yn, Zn} /= {X, Y, Z}].

neigbours4d({X, Y, Z, W}, Map) ->
    [maps:get({Xn, Yn, Zn, Wn}, Map, wall)
     || Xn <- lists:seq(X - 1, X + 1),
        Yn <- lists:seq(Y - 1, Y + 1),
        Zn <- lists:seq(Z - 1, Z + 1),
        Wn <- lists:seq(W - 1, W + 1),
        {Xn, Yn, Zn, Wn} /= {X, Y, Z, W}].
