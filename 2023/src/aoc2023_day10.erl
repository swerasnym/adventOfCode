-module(aoc2023_day10).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day10.txt"),
    io:format("S1: ~p ~nS2: ~p ~n", [S1, S2]),
    Res.

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

star1({Start, Map}) ->
    {Count, _} = move([Start], Map, 0, []),
    Count.

star2({Start, Map}) ->
    {_, MapOut} = move([Start], Map, 0, []),
    SimplifiedMap = #{Pos => simplify(Val) || Pos := Val <- MapOut},
    lists:sum([count_row(0, Row, outside) || Row <- tools:grid_to_lists(SimplifiedMap)]).

read(File) ->
    Map = tools:read_grid(File, #{
        $S => start,
        $. => empty,
        $| => [north, south],
        $L => [north, east],
        $J => [north, west],
        $7 => [south, west],
        $F => [south, east],
        $- => [east, west],
        %% To be able to parse examples with inside/outside marked.
        $I => empty,
        $O => empty
    }),
    [Start] = [P || P := start <- Map],
    StartDirs = [
        D
     || D <- [north, south, east, west],
        lists:member(Start, neigbours(get_neigbour(Start, D), Map))
    ],
    %% Asert that only one pipe pice is valid
    2 = length(StartDirs),
    {Start, Map#{Start := StartDirs}}.

neigbours(Pos, Map) when is_map(Map) ->
    [get_neigbour(Pos, Dir) || Dir <- maps:get(Pos, Map)];
neigbours(Pos, Dirs) when is_list(Dirs) ->
    [get_neigbour(Pos, Dir) || Dir <- Dirs];
neigbours(_, _) ->
    [].

get_neigbour({X, Y}, north) -> {X, Y - 1};
get_neigbour({X, Y}, south) -> {X, Y + 1};
get_neigbour({X, Y}, east) -> {X + 1, Y};
get_neigbour({X, Y}, west) -> {X - 1, Y}.

move([], Map, Count, []) ->
    {Count, Map};
move([], Map, Count, NextLayer) ->
    move(NextLayer, Map, Count + 1, []);
move([Pos | Rest], Map, Count, NextLayer) ->
    case maps:get(Pos, Map) of
        {visited, _} ->
            move(Rest, Map, Count, NextLayer);
        Dirs when Dirs /= empty ->
            New = [N || N <- neigbours(Pos, Dirs), is_list(maps:get(N, Map))],
            move(Rest, Map#{Pos := {visited, Dirs}}, Count, New ++ NextLayer)
    end.

count_row(Count, [], _) ->
    Count;
count_row(Count, [empty | Rest], outside) ->
    count_row(Count, Rest, outside);
count_row(Count, [empty | Rest], inside) ->
    count_row(Count + 1, Rest, inside);
count_row(Count, [flip | Rest], IO) ->
    count_row(Count, Rest, flip(IO));
count_row(Count, [ignore | Rest], IO) ->
    count_row(Count, Rest, IO).

flip(outside) -> inside;
flip(inside) -> outside.

simplify({visited, [north, _]}) ->
    %% Its enough to check that a pipe has a notthen exit to count boundaries from the outside.
    flip;
simplify({visited, _}) ->
    ignore;
simplify(_) ->
    empty.
