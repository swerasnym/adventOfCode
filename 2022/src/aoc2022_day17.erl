-module(aoc2022_day17).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day17.txt"),
    io:format("S1: ~p ~nS2: ~p ~n", [S1, S2]),
    Res.

run(Star, File) ->
    erlang:erase(),
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

-define(STOP1, 2022).
-define(STOP2, 1000000000000).

read(File) ->
    tools:read_lines(File).

star1([Movements]) ->
    Well = tools:drop_max(tools:lists_to_grid(["FFFFFFF"])),
    End = simulate(0, ?STOP1, Well, Movements, Movements),
    %%ools:print_grid(End),
    {{0, 6}, {NegHeight, _}} = tools:minmax_grid(End),
    -NegHeight.

star2([Movements]) ->
    Well = tools:drop_max(tools:lists_to_grid(["FFFFFFF"])),
    End = simulate(0, ?STOP2, Well, Movements, Movements),
    %%tools:print_grid(End),
    {{0, 6}, {NegHeight, _}} = tools:minmax_grid(End),
    -NegHeight.

%% Ponints to form a rock in starting configuration with its lowest point at 0,
%% A point closest to the left edge first, A point closest to the right edge last.
rock(0) ->
    [{2, 0}, {3, 0}, {4, 0}, {5, 0}];
rock(1) ->
    [{2, -1}, {3, -2}, {3, -1}, {3, 0}, {4, -1}];
rock(2) ->
    [{2, 0}, {4, -2}, {4, -1}, {3, 0}, {4, 0}];
rock(3) ->
    [{2, -3}, {2, -2}, {2, -1}, {2, 0}];
rock(4) ->
    [{2, -1}, {3, -1}, {2, 0}, {3, 0}];
rock(N) ->
    rock(N rem 5).

%% Each rock appears so that its left edge is two units away from the left wall and its bottom edge
%% is three units above the highest rock in the room

start(Rock, Well) ->
    {_, {Ymin, _}} = tools:minmax_grid(Well),
    Dy = Ymin - 4,
    [{X, Y + Dy} || {X, Y} <- rock(Rock)].

stopped(Points) ->
    maps:from_list([{P, $#} || P <- Points]).

stopped(Points, N) ->
    maps:from_list([{P, $0 + N rem 10} || P <- Points]).

falling(Points) ->
    maps:from_list([{P, $@} || P <- Points]).

overlapps(Points, Well) ->
    lists:min([maps:get(P, Well, empty) || P <- Points]) /= empty.

move_horisontally(Points, $>) ->
    case lists:last(Points) of
        {6, _} ->
            Points;
        _ ->
            [{X + 1, Y} || {X, Y} <- Points]
    end;
move_horisontally(Points, $<) ->
    case hd(Points) of
        {0, _} ->
            Points;
        _ ->
            [{X - 1, Y} || {X, Y} <- Points]
    end.

move(Points, Well, Dir) ->
    MovedH = move_horisontally(Points, Dir),
    MovedV = [{X, Y + 1} || {X, Y} <- Points],
    MovedHV = [{X, Y + 1} || {X, Y} <- MovedH],

    case {overlapps(MovedH, Well), overlapps(MovedV, Well), overlapps(MovedHV, Well)} of
        {true, true, _} ->
            {stopped, Points};
        {true, false, _} ->
            {continue, MovedV};
        {false, _, true} ->
            {stopped, MovedH};
        {false, _, false} ->
            {continue, MovedHV}
    end.

simulate(Stop, Stop, Well, _, _) ->
    Well;
simulate(RockN, Stop, Well, Movements, AllMovements) ->
    {_, Offset} = lists:min(maps:keys(Well)),

    State =
        {
            lists:sort([{X, Y - Offset} || {X, Y} <- maps:keys(Well)]),
            RockN rem 5,
            length(Movements)
        },
    case erlang:get(State) of
        undefined ->
            erlang:put(State, {RockN, Offset}),
            %% io:format("~p~n", [RockN]),
            %% tools:print_grid(Well),
            Rock = start(RockN, Well),
            {Rock1, Rest} = simulate_rock(Rock, Well, Movements, AllMovements),
            NextWell = maps:merge(Well, stopped(Rock1, RockN)),
            Floor = find_floor(NextWell),

            %% tools:print_grid(Floor),
            simulate(RockN + 1, Stop, Floor, Rest, AllMovements);
        {LastRockN, LastOffset} ->
            %% remove states to not end in an infinite loop
            erlang:erase(),
            Distance = RockN - LastRockN,
            RocksLeft = Stop - RockN,
            LoopsLeft = RocksLeft div Distance,
            Next = LoopsLeft * Distance + RockN,
            DOffset = Offset - LastOffset,
            NextOffset = DOffset * LoopsLeft,

            Floor = stopped([{X, Y + NextOffset} || {X, Y} <- maps:keys(Well)]),

            simulate(Next, Stop, Floor, Movements, AllMovements)
    end.

simulate_rock(Rock, Well, [], AllMovements) ->
    simulate_rock(Rock, Well, AllMovements, AllMovements);
simulate_rock(Rock, Well, [Dir | Rest], AllMovements) ->
    case move(Rock, Well, Dir) of
        {stopped, Rock1} ->
            {Rock1, Rest};
        {continue, Rock1} ->
            simulate_rock(Rock1, Well, Rest, AllMovements)
    end.

find_floor(Well) ->
    Leftmost = lists:sort([P || P = {X, _} <- maps:keys(Well), X == 0]),
    Floor = find_floor(Leftmost, Well),

    LowestPartsOfFloor =
        [lists:max([P || P = {PX, _} <- Floor, PX == X]) || X <- lists:seq(0, 6)],
    AboveFloor = lists:filter(fun(P) -> above(P, LowestPartsOfFloor) end, maps:keys(Well)),
    maps:merge(stopped(Floor), falling(AboveFloor)).

find_floor([P | Rest], Well) ->
    case find_floor(P, Well) of
        none ->
            find_floor(Rest, Well);
        Floor ->
            Floor
    end;
find_floor(P, Well) ->
    find_floor(neigbours(P, Well), Well, [P], [P]).

find_floor([], _Well, _Floor, _) ->
    none;
find_floor([P = {6, _} | _], _Well, Floor, _Tested) ->
    [P | Floor];
find_floor([P | Rest], Well, Floor, Tested) ->
    case find_floor(neigbours(P, Well) -- Tested, Well, [P | Floor], [P | Tested]) of
        none ->
            find_floor(Rest, Well, Floor, [P | Tested]);
        Floor1 ->
            Floor1
    end.

above({X, Y}, Floor) ->
    Y < lists:min([Yf || {Xf, Yf} <- Floor, Xf == X]).

neigbours(P, Well) ->
    [N || N <- neigbours(P), maps:is_key(N, Well)].

neigbours({X, Y}) ->
    [
        {X + Dx, Y + Dy}
     || Dx <- [1, 0, -1], Dy <- [-1, 0, 1], {Dx, Dy} /= {0, 0}, Dx * Dy == 0
    ].
