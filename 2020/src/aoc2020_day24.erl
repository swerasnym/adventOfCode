-module(aoc2020_day24).

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
    tools:count(black, lists:foldl(fun flip/2, #{}, Data)).

star2(Data) ->
    Floor = lists:foldl(fun flip/2, #{}, Data),
    BlackTiles = maps:from_list([{Pos, black} || {Pos, black} <- maps:to_list(Floor)]),
    maps:size(iterate(100, BlackTiles)).

read(File) ->
    [tokenize(Line) || Line <- tools:read_lines(File)].

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

iterate(0, Floor) ->
    Floor;
iterate(N, Floor) ->
    NewPos = lists:usort(lists:flatmap(fun neigbours/1, maps:keys(Floor))),

    Update =
        fun(Pos, Acc) ->
            Neigbours = [Neigbour || Neigbour <- neigbours(Pos), maps:is_key(Neigbour, Floor)],

            case {maps:get(Pos, Floor, white), length(Neigbours)} of
                {black, 1} ->
                    Acc#{Pos => black};
                {black, 2} ->
                    Acc#{Pos => black};
                {white, 2} ->
                    Acc#{Pos => black};
                _ ->
                    Acc
            end
        end,
    iterate(N - 1, lists:foldl(Update, #{}, NewPos)).

neigbours(Pos) ->
    [move(Pos, Dir) || Dir <- [e, se, sw, w, nw, ne]].
