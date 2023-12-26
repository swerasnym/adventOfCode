-module(aoc2022_day18).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 18}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    maps:from_list([{list_to_tuple(L), $#} || L <- tools:read_format(File, "~d,~d,~d")]).

star1(Data) ->
    maps:fold(
        fun(P, _, Acc) ->
            Acc + 6 - length([N || N <- neigbours(P), maps:get(N, Data, none) /= none])
        end,
        0,
        Data
    ).

star2(Data) ->
    B = {{Xmin, _Xmax}, {Ymin, _Ymax}, {Zmin, _Zmax}} = bounds(Data),
    Marked = mark_outside([{Xmin, Ymin, Zmin}], Data, B, [{Xmin, Ymin, Zmin}]),
    maps:fold(
        fun
            (P, $#, Acc) ->
                Acc + length([N || N <- neigbours(P), maps:get(N, Marked, none) == outside]);
            (_, _, Acc) ->
                Acc
        end,
        0,
        Marked
    ).

neigbours({X, Y, Z}) ->
    [
        {X + 1, Y, Z},
        {X - 1, Y, Z},
        {X, Y + 1, Z},
        {X, Y - 1, Z},
        {X, Y, Z + 1},
        {X, Y, Z - 1}
    ].

bounds(Map) ->
    {Xs, Ys, Zs} = lists:unzip3(maps:keys(Map)),
    {{lists:min(Xs) - 1, lists:max(Xs) + 1}, {lists:min(Ys) - 1, lists:max(Ys) + 1}, {
        lists:min(Zs) - 1, lists:max(Zs) + 1
    }}.

mark_outside([], Map, _, _) ->
    Map;
mark_outside([P | Rest], Map, {{Xmin, Xmax}, {Ymin, Ymax}, {Zmin, Zmax}} = Bounds, V) ->
    case maps:get(P, Map, unmarked) of
        unmarked ->
            NewNeigbours =
                [
                    N
                 || N = {X, Y, Z} <- neigbours(P),
                    X >= Xmin,
                    X =< Xmax,
                    Y >= Ymin,
                    Y =< Ymax,
                    Z >= Zmin,
                    Z =< Zmax
                ] --
                    V,

            mark_outside(Rest ++ NewNeigbours, Map#{P => outside}, Bounds, V ++ NewNeigbours);
        _ ->
            mark_outside(Rest, Map, Bounds, V)
    end.
