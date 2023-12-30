-module(aoc2018_day13).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).
-record(cart, {pos, dir, turns = 0}).

info() ->
    Examples = [
        {"examples/2018/day13_ex.txt", star1, "7,3"},
        {"examples/2018/day13_ex2.txt", star2, "6,4"},
        {"examples/2018/day13_ex3.txt", star1, "2,0"},
        {"examples/2018/day13_ex3.txt", star2, "4,0"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 13},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Carts, RailMap}) ->
    {X, Y} = simulate([], RailMap, Carts, crash),
    lists:flatten(io_lib:format("~p,~p", [X, Y])).

star2({Carts, RailMap}) ->
    {X, Y} = simulate([], RailMap, Carts, cart),
    lists:flatten(io_lib:format("~p,~p", [X, Y])).

read(File) ->
    Grid0 = tools:read_grid(File, #{
        $\s => empty,
        $/ => corner,
        $\\ => corner,
        $| => [north, south],
        $- => [east, west],
        $+ => [north, south, east, west],
        $^ => {cart, north},
        $v => {cart, south},
        $> => {cart, east},
        $< => {cart, west}
    }),

    Carts = [#cart{pos = P, dir = D} || P := {cart, D} <- Grid0],
    Grid1 = maps:map(fun(_, V) -> to_edge(V) end, Grid0),
    Grid2 = maps:map(fun(K, V) -> to_edge(Grid1, K, V) end, Grid1),
    {Carts, maps:filter(fun(_, V) -> V /= empty end, Grid2)}.
simulate([], _RailMap, [#cart{pos = Pos}], cart) ->
    Pos;
simulate([], RailMap, Carts, End) ->
    simulate(lists:sort(fun cmp_carts/2, Carts), RailMap, [], End);
simulate([Cart0 | Carts], RailMap, NewCarts, End) ->
    #cart{pos = Pos} = Cart1 = move(Cart0, RailMap),
    Coitions = [C || #cart{pos = P} = C <- Carts ++ NewCarts, P == Pos],
    case {Coitions, End} of
        {[], _} ->
            simulate(Carts, RailMap, [Cart1 | NewCarts], End);
        {[_], crash} ->
            Pos;
        {[C], cart} ->
            simulate(Carts -- [C], RailMap, NewCarts -- [C], End)
    end.

cmp_carts(#cart{pos = {X1, Y1}}, #cart{pos = {X2, Y2}}) ->
    {Y1, X1} < {Y2, X2}.
% {X1, Y1} < {X2, Y2}.

to_edge({cart, D}) ->
    case D of
        north -> [north, south];
        south -> [north, south];
        east -> [east, west];
        west -> [east, west]
    end;
to_edge(NonCart) ->
    NonCart.

to_edge(Map, {X, Y}, corner) ->
    [
        case maps:get({X, Y - 1}, Map, empty) of
            L when is_list(L) ->
                case lists:member(south, L) of
                    true -> north;
                    false -> south
                end;
            _ ->
                south
        end,
        case maps:get({X - 1, Y}, Map, empty) of
            L when is_list(L) ->
                case lists:member(east, L) of
                    true -> west;
                    false -> east
                end;
            _ ->
                east
        end
    ];
to_edge(_, _, NonCorner) ->
    NonCorner.

turn(#cart{dir = north, turns = T}) ->
    case T rem 3 of
        0 -> west;
        1 -> north;
        2 -> east
    end;
turn(#cart{dir = east, turns = T}) ->
    case T rem 3 of
        0 -> north;
        1 -> east;
        2 -> south
    end;
turn(#cart{dir = south, turns = T}) ->
    case T rem 3 of
        0 -> east;
        1 -> south;
        2 -> west
    end;
turn(#cart{dir = west, turns = T}) ->
    case T rem 3 of
        0 -> south;
        1 -> west;
        2 -> north
    end.

opposite(north) -> [south];
opposite(south) -> [north];
opposite(east) -> [west];
opposite(west) -> [east].

next_pos(#cart{pos = {X, Y}, dir = north}) -> {X, Y - 1};
next_pos(#cart{pos = {X, Y}, dir = south}) -> {X, Y + 1};
next_pos(#cart{pos = {X, Y}, dir = east}) -> {X + 1, Y};
next_pos(#cart{pos = {X, Y}, dir = west}) -> {X - 1, Y}.

move(#cart{dir = Dir, turns = T} = Cart, RailMap) ->
    NextPos = next_pos(Cart),
    case maps:get(NextPos, RailMap) of
        L when is_list(L), length(L) == 2 ->
            Cart#cart{pos = NextPos, dir = hd(L -- opposite(Dir))};
        _Crossing ->
            Cart#cart{pos = NextPos, dir = turn(Cart), turns = T + 1}
    end.
