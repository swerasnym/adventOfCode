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

star1(Map) ->
    erlang:erase(),
    Start = [P || P := [north, south, east, west] <- Map],
    move(Start, Map, 0, []),
    erlang:get(dist).

star2(#{max := {XMax, YMax}} = Map) ->
    io:format("~p~n", [{XMax, YMax}]),
    erlang:erase(),
    Start = [P || P := [north, south, east, west] <- Map],
    MapOut = move(Start, Map, 0, []),
    SimplifiedMap = maps:map(fun(P, Val) -> transform(P, Val, Map) end, MapOut),

    % [io:format("~p~n" , [Row]) || Row <- tools:grid_to_lists(SimplifiedMap)],
    lists:sum([count_row(0, Row, outside) || Row <- tools:grid_to_lists(SimplifiedMap)]).

read(File) ->
    tools:read_grid(File, #{
        $S => [north, south, east, west],
        $. => empty,
        $| => [north, south],
        $- => [east, west],
        $L => [north, east],
        $J => [north, west],
        $7 => [south, west],
        $F => [south, east],
        %% To be able to parse examples with IO
        $I => empty,
        $O => empty
    }).

get_neigbour({X, Y}, north) -> {X, Y - 1};
get_neigbour({X, Y}, south) -> {X, Y + 1};
get_neigbour({X, Y}, east) -> {X + 1, Y};
get_neigbour({X, Y}, west) -> {X - 1, Y}.

move([], Map, Count, []) ->
    Map;
move([], Map, Count, NextLayer) ->
    % io:format("~p ~p~n", [Count+1, NextLayer]),
    move(NextLayer, Map, Count + 1, []);
move([Pos | Rest], Map, Count, NextLayer) ->
    % io:format("~p ~p~n", [Count, Pos]),
    case maps:get(Pos, Map, empty) of
        empty ->
            move(Rest, Map, Count, NextLayer);
        {visited, Count} ->
            move(Rest, Map, Count, NextLayer);
        {visited, N} when N == Count - 1 ->
            move(Rest, Map, Count, NextLayer);
        {visited, _} ->
            move(Rest, Map, Count, NextLayer);
        Dirs ->
            Neigbours = [get_neigbour(Pos, D) || D <- Dirs],
            NFilter = [
                N
             || N <- Neigbours,
                is_list(NDirs = maps:get(N, Map, empty)),
                lists:member(Pos, [get_neigbour(N, ND) || ND <- NDirs])
            ],
            %  io:format("~p ~p ~p ~p ~p~n", [Count, Pos, Dirs, Neigbours, NFilter]),
            erlang:put(dist, Count),
            move(Rest, Map#{Pos := {visited, Count}}, Count, NFilter ++ NextLayer)
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
    count_row(Count, Rest, IO);
count_row(Count, [Dir, Dir | Rest], IO) ->
    count_row(Count, Rest, IO);
count_row(Count, [Dir, ignore | Rest], IO) ->
    count_row(Count, [Dir | Rest], IO);
count_row(Count, [up, down | Rest], IO) ->
    count_row(Count, Rest, flip(IO));
count_row(Count, [down, up | Rest], IO) ->
    count_row(Count, Rest, flip(IO)).

flip(outside) -> inside;
flip(inside) -> outside.

transform(Pos, {visited, _}, Map) ->
    case maps:get(Pos, Map) of
        [north, south, east, west] = Dirs ->
            Neigbours = [{get_neigbour(Pos, D), D} || D <- Dirs],
            NewD = [
                D
             || {N, D} <- Neigbours,
                is_list(NDirs = maps:get(N, Map, empty)),
                lists:member(Pos, [get_neigbour(N, ND) || ND <- NDirs])
            ],

            transform(Pos, {visited, 0}, Map#{Pos := NewD});
        [north, south] ->
            flip;
        [east, west] ->
            ignore;
        [north, _] ->
            up;
        [south, _] ->
            down
    end;
transform(_, _, _) ->
    empty.
