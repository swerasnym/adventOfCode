-module(aoc2019_day10).

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
    {Asterioids, _} = lists:max(hits(Data)),
    Asterioids.

star2(Data) ->
    {_, Base} = lists:max(hits(Data)),
    Hits = vaporize(Base, Data),
    Map = maps:from_list(
              lists:zip(
                  lists:seq(1, length(Hits)), Hits)),
    {X, Y} = maps:get(200, Map, none),
    X * 100 + Y.

read(File) ->
    {ok, Device} = file:open(File, [read]),
    read(Device, 0, #{}).

read(Device, Row, Acc) ->
    case file:read_line(Device) of
        eof ->
            Acc;
        {ok, Line0} ->
            Fold = fun({Elem, Column}, AccIn) -> AccIn#{{Column, Row} => Elem} end,
            Line = string:trim(Line0),
            read(Device,
                 Row + 1,
                 lists:foldl(Fold, Acc, lists:zip(Line, lists:seq(0, length(Line) - 1))))
    end.

dim(Data) ->
    lists:max(
        maps:keys(Data)).

directions(Data) ->
    {Rows, Cols} = dim(Data),
    [{C, R} || C <- lists:seq(1, Cols), R <- lists:seq(1, Rows), gcd(R, C) == 1].

hits(Data) ->
    [hits(Base, Data) || Base <- maps:keys(Data)].

hits(Base, Data) ->
    case maps:get(Base, Data) of
        $. ->
            {0, Base};
        $# ->
            Dirs = directions(Data),
            Hits =
                lists:sum([hits(Base, {DC, DR}, Data)
                           + hits(Base, {-DC, DR}, Data)
                           + hits(Base, {DC, -DR}, Data)
                           + hits(Base, {-DC, -DR}, Data)
                           || {DC, DR} <- Dirs]),

            Hits2 = lists:sum([hits(Base, Dir, Data) || Dir <- [{0, 1}, {1, 0}, {-1, 0}, {0, -1}]]),
            {Hits + Hits2, Base}
    end.

hits(Base, Dir, Data) ->
    hits(Base, Base, Dir, Data).

hits(Base, {Column, Row}, {DC, DR} = Dir, Data) ->
    case maps:get({Column + DC, Row + DR}, Data, outside) of
        outside ->
            0;
        $# ->
            1;
        $. ->
            hits(Base, {Column + DC, Row + DR}, Dir, Data)
    end.

hits2({Column, Row}, {DC, DR} = Dir, Data) ->
    case maps:get({Column + DC, Row - DR}, Data, outside) of
        outside ->
            none;
        $# ->
            {Column + DC, Row - DR};
        $. ->
            hits2({Column + DC, Row - DR}, Dir, Data)
    end.

gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).

dirs(Data) ->
    Angle = lists:sort([{C / R, Pos} || {C, R} = Pos <- directions(Data)]),

    Q1 = [{C, R} || {_, {C, R}} <- Angle],
    Q4 = [{C, -R} || {_, {C, R}} <- lists:reverse(Angle)],
    Q3 = [{-C, -R} || {_, {C, R}} <- Angle],
    Q2 = [{-C, R} || {_, {C, R}} <- lists:reverse(Angle)],

    [{0, 1}] ++ Q1 ++ [{1, 0}] ++ Q4 ++ [{0, -1}] ++ Q3 ++ [{-1, 0}] ++ Q2.

vaporize(Base, Data) ->
    Dirs = dirs(Data),

    vaporize(Base, Data, Dirs, Dirs, [], []).

vaporize(Base, Data, [], AllDirs, Acc, Last) ->
    case Acc of
        Last ->
            lists:reverse(Acc);
        _ ->
            vaporize(Base, Data, AllDirs, AllDirs, Acc, Acc)
    end;
vaporize(Base, Data, [Dir | Dirs], AllDirs, Acc, Last) ->
    case hits2(Base, Dir, Data) of
        none ->
            vaporize(Base, Data, Dirs, AllDirs, Acc, Last);
        Pos ->
            vaporize(Base, Data#{Pos => $.}, Dirs, AllDirs, [Pos | Acc], Last)
    end.
