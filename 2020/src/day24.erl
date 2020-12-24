-module(day24).

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
    maps:get(black,
             count(maps:values(
                       lists:foldl(fun flip/2, #{}, Data)))).

star2(Data) ->
    Floor = lists:foldl(fun flip/2, #{}, Data),
    Day100 = update(100, Floor),
    maps:get(black, count(maps:values(Day100))).

read(File) ->
    {ok, Bin} = file:read_file(File),
    [tokenize(Line)
     || Line
            <- string:split(
                   string:trim(binary_to_list(Bin)), "\n", all)].

tokenize(Line) ->
    tokenize(Line, []).

tokenize([], Acc) ->
    lists:reverse(Acc);
tokenize("e" ++ Rest, Acc) ->
    tokenize(Rest, [e | Acc]);
tokenize("se" ++ Rest, Acc) ->
    tokenize(Rest, [se | Acc]);
tokenize("sw" ++ Rest, Acc) ->
    tokenize(Rest, [sw | Acc]);
tokenize("w" ++ Rest, Acc) ->
    tokenize(Rest, [w | Acc]);
tokenize("nw" ++ Rest, Acc) ->
    tokenize(Rest, [nw | Acc]);
tokenize("ne" ++ Rest, Acc) ->
    tokenize(Rest, [ne | Acc]).

move({X, Y}, e) ->
    {X + 2, Y};
move({X, Y}, se) ->
    {X + 1, Y - 1};
move({X, Y}, sw) ->
    {X - 1, Y - 1};
move({X, Y}, w) ->
    {X - 2, Y};
move({X, Y}, nw) ->
    {X - 1, Y + 1};
move({X, Y}, ne) ->
    {X + 1, Y + 1}.

flip(white) ->
    black;
flip(black) ->
    white.

flip(Dirs, Map) ->
    flip({0, 0}, Dirs, Map).

flip(Pos, [], Map) ->
    maps:update_with(Pos, fun flip/1, black, Map);
flip(Pos, [Dir | Dirs], Map) ->
    flip(move(Pos, Dir), Dirs, Map).

update(0, Floor) ->
    Floor;
update(N, Floor) ->
    NewPos =
        lists:usort(
            lists:flatten([neigbours(Pos) || {Pos, black} <- maps:to_list(Floor)])),
    update(N - 1, maps:from_list([update_tile(Pos, Floor) || Pos <- NewPos])).

update_tile(Pos, Floor) ->
    Neigbours = count([maps:get(N, Floor, white) || N <- neigbours(Pos)]),

    %% Zero black neigbours are taken care of by only considering neigbours to a black tile
    case {maps:get(Pos, Floor, white), maps:get(black, Neigbours)} of
        {black, N} when N > 2 ->
            {Pos, white};
        {white, 2} ->
            {Pos, black};
        {Color, _} ->
            {Pos, Color}
    end.

neigbours(Pos) ->
    [move(Pos, Dir) || Dir <- [e, se, sw, w, nw, ne]].

count(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end, #{}, List).
