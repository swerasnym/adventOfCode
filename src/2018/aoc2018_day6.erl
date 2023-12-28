-module(aoc2018_day6).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, star2/2, read/1]).

info() ->
    Examples = [
        {"examples/2018/day6_ex.txt", star1, 17},
        {"examples/2018/day6_ex.txt", {star2, 32}, 16}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 6},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Points) ->
    Map = #{P => {V + $a - 1, 0} || {V, P} <- lists:enumerate(Points)},
    Limits = tools:min_max_grid(Map),
    Start = [{P, V} || P := {V, 0} <- Map],

    {Filled, Outside} = fill_space(Start, [], Map, Limits, #{}, 0),

    Grid = #{P => V || P := {V, _} <- Filled},

    Inside = [V || {_, V} <- Start, not maps:is_key(V, Outside)],
    % io:format("~p~n", [Inside]),
    % tools:print_grid(Grid),
    Counts = tools:count(Grid),

    Sizes = [maps:get(I, Counts) || I <- Inside],
    % io:format("~p~n", [Sizes]),
    lists:max(Sizes).

star2(Points) ->
    star2(Points, 10000).

star2(Points, MaxSum) ->
    {Xs, Ys} = lists:unzip(Points),
    Xmin = lists:min(Xs),
    Ymin = lists:min(Ys),
    Xt = lists:sort([X - Xmin || X <- Xs]),
    Yt = lists:sort([Y - Ymin || Y <- Ys]),
    XSum = lists:sum(Xt),
    YSum = lists:sum(Yt),
    %io:format("~p ~p~n", [XSum, YSum]),

    StartX = (XSum - MaxSum) div length(Xt),
    StartY = (YSum - MaxSum) div length(Yt),

    %io:format("~p ~p~n", [StartX, StartY]),

    XPoints = move(StartX, XSum - StartX * length(Xt), 0, length(Xt), Xt, [], MaxSum),
    YPoints = move(StartY, YSum - StartY * length(Yt), 0, length(Yt), Yt, [], MaxSum),

    length([X + Y || X <- XPoints, Y <- YPoints, X + Y < MaxSum]).

read(File) ->
    [list_to_tuple(P) || P <- tools:read_multiple_formats(File, "~d, ~d")].

fill_space([], [], Map, _Limits, Outside, _Dist) ->
    {Map, Outside};
fill_space([], New, Map, Limits, Outside, Dist) ->
    fill_space(New, [], Map, Limits, Outside, Dist + 1);
fill_space([{P, V} | Rest], New, Map, Limits, Outside, Dist) ->
    Neighbours = [{N, V} || N <- neighbours(P, Limits), N /= outside],
    case length(Neighbours) of
        4 ->
            NewOutside = Outside;
        _ ->
            NewOutside = Outside#{V => true}
    end,
    % io:format("{~p, ~c} ~p ~n", [P, V, neighbours(P, Limits)]),

    case maps:get(P, Map, empty) of
        empty ->
            fill_space(Rest, New ++ Neighbours, Map#{P => {V, Dist}}, Limits, NewOutside, Dist);
        {V, 0} ->
            fill_space(Rest, New ++ Neighbours, Map#{P => {V, Dist}}, Limits, NewOutside, Dist);
        {V, _} ->
            fill_space(Rest, New, Map, Limits, NewOutside, Dist);
        {_, N} when N == Dist ->
            fill_space(Rest, New, Map#{P => {$., Dist}}, Limits, NewOutside, Dist);
        {_, N} when N < Dist ->
            fill_space(Rest, New, Map, Limits, NewOutside, Dist)
    end.

neighbours(P, Limits) ->
    [neighbour(D, P, Limits) || D <- [north, south, east, west]].

neighbour(north, {X, Y}, {_, {Ymin, _}}) ->
    case Y == Ymin - 1 of
        true ->
            outside;
        false ->
            {X, Y - 1}
    end;
neighbour(south, {X, Y}, {_, {_, Ymax}}) ->
    case Y == Ymax + 1 of
        true ->
            outside;
        false ->
            {X, Y + 1}
    end;
neighbour(east, {X, Y}, {{Xmin, _}, _}) ->
    case X == Xmin - 1 of
        true ->
            outside;
        false ->
            {X - 1, Y}
    end;
neighbour(west, {X, Y}, {{_, Xmax}, _}) ->
    case X == Xmax + 1 of
        true ->
            outside;
        false ->
            {X + 1, Y}
    end.

move(_Pos, Sum, _Left, 0, _Points, Acc, Limit) when Sum > Limit ->
    Acc;
move(Pos, Sum, Left, Right, [Pos | Rest], Acc, Limit) ->
    move(
        Pos,
        Sum,
        Left + 1,
        Right - 1,
        Rest,
        Acc,
        Limit
    );
move(Pos, Sum, Left, Right, Points, Acc, Limit) ->
    move(
        Pos + 1,
        Sum + Left - Right,
        Left,
        Right,
        Points,
        [Sum | Acc],
        Limit
    ).
