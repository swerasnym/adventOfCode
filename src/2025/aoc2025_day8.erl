-module(aoc2025_day8).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2025/day8_ex.txt", {star1, 10}, 40},
        {"examples/2025/day8_ex.txt", star2, 25272}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2025, 8},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    star1(Data, 1000).

star1({Boxes, DistancesIn}, N) ->
    Distances = lists:sublist(DistancesIn, N),

    {_, Networks} = build_circuits(Boxes, Distances),

    Sizes = tools:reverse_sort([
        length(maps:get(X, Networks, []))
     || X <- lists:seq(1, length(Boxes))
    ]),
    tools:product(lists:sublist(Sizes, 3)).

star2({Boxes, Distances}) ->
    {Last, _} = build_circuits(Boxes, Distances),
    {{X1, _, _}, {X2, _, _}} = Last,
    X1 * X2.

read(File) ->
    Boxes = tools:group(3, tools:read_integers(File, ",\n")),
    {Boxes, distances(Boxes)}.

distance2({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    X = X1 - X2,
    Y = Y1 - Y2,
    Z = Z1 - Z2,
    X * X + Y * Y + Z * Z.

distances(Boxes) ->
    lists:sort([{distance2(B1, B2), B1, B2} || B1 <- Boxes, B2 <- Boxes, B1 < B2]).

build_circuits(Boxes, Distances) ->
    Circuits = maps:merge(
        #{I => [Box] || {I, Box} <- lists:enumerate(Boxes)},
        #{Box => I || {I, Box} <- lists:enumerate(Boxes)}
    ),

    build_circuits(Distances, Circuits, {0, 0}).

build_circuits([], Circuits, Last) ->
    {Last, Circuits};
build_circuits([{_, A, B} | Rest], Circuits, Last) ->
    case [maps:get(A, Circuits), maps:get(B, Circuits)] of
        [X, X] ->
            build_circuits(Rest, Circuits, Last);
        [X, Y] ->
            XList = maps:get(X, Circuits),
            YList = maps:get(Y, Circuits),
            Moved = #{Box => X || Box <- YList},
            XNew = XList ++ YList,

            build_circuits(
                Rest, maps:merge(Circuits#{X => XNew, Y => []}, Moved), {A, B}
            )
    end.
