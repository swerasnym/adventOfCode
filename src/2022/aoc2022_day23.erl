-module(aoc2022_day23).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 23}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    tools:read_grid(File).

star1(Map) ->
    Map10 = tools:repeat(10, fun step/2, Map),
    {{MinX, MaxX}, {MinY, MaxY}} = tools:min_max_grid(Map10),
    (MaxX - MinX + 1) * (MaxY - MinY + 1) - tools:count($#, Map10).

star2(Map) ->
    same(1, Map).

same(N, Map) ->
    Next = step(N, Map),
    case Map == Next of
        true ->
            tools:print_grid(Next),
            N;
        false ->
            same(N + 1, Next)
    end.

step(N, Map) ->
    Prop = lists:sort([{decide(N, P, Map), P} || {P, $#} <- maps:to_list(Map)]),
    Move = avoid_coitions(Prop, []),
    maps:from_keys(Move, $#).

avoid_coitions([{P, A}, {P, B} | Rest], Acc) ->
    avoid_coitions(Rest, [A, B | Acc]);
avoid_coitions([{P, _} | Rest], Acc) ->
    avoid_coitions(Rest, [P | Acc]);
avoid_coitions([], Acc) ->
    Acc.

decide(Step, {X, Y} = P, Map) ->
    case [maps:get(N, Map, $.) || N <- neighbours(P)] of
        "........" ->
            P;
        [NW, N, NE, W, E, SW, S, SE] ->
            move(
                P,
                tools:rotate(
                    Step,
                    [
                        {[E, NE, SE], {X + 1, Y}},
                        {[N, NW, NE], {X, Y - 1}},
                        {[S, SW, SE], {X, Y + 1}},
                        {[W, NW, SW], {X - 1, Y}}
                    ]
                )
            )
    end.

move(P, []) ->
    P;
move(_P, [{"...", To} | _]) ->
    To;
move(P, L) ->
    move(P, tl(L)).

neighbours({X, Y}) ->
    [
        {X - 1, Y - 1},
        {X, Y - 1},
        {X + 1, Y - 1},
        {X - 1, Y},
        {X + 1, Y},
        {X - 1, Y + 1},
        {X, Y + 1},
        {X + 1, Y + 1}
    ].
