-module(aoc2023_day17).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"2023/data/day17_ex.txt", star1, 102},
        {"2023/data/day17_ex.txt", star2, 94}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 17},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(#{max := End} = Map) ->
    Start = gb_sets:from_list([{0, {{0, 0}, start, 0}}]),
    search(Start, End, Map, #{}, fun move1/2).

star2(#{max := End} = Map) ->
    Start = gb_sets:from_list([{0, {{0, 0}, start, 0}}]),
    search(Start, End, Map, #{}, fun move2/2).

read(File) ->
    tools:read_grid(File, fun(D) -> D - $0 end).

get_neigbour({X, Y}, north) -> {X, Y - 1};
get_neigbour({X, Y}, south) -> {X, Y + 1};
get_neigbour({X, Y}, east) -> {X + 1, Y};
get_neigbour({X, Y}, west) -> {X - 1, Y}.

get_turns(start) ->
    [south, east];
get_turns(north) ->
    [east, west];
get_turns(south) ->
    [east, west];
get_turns(east) ->
    [north, south];
get_turns(west) ->
    [north, south].

next({Pos, Dir, 0}, Max, _Min) ->
    [{get_neigbour(Pos, D), D, Max - 1} || D <- get_turns(Dir)];
next({Pos, Dir, N}, Max, Min) when N > Max - Min, Dir /= start ->
    [{get_neigbour(Pos, Dir), Dir, N - 1}];
next({Pos, Dir, N}, Max, _Min) when N > 0, Dir /= start ->
    [{get_neigbour(Pos, D), D, Max - 1} || D <- get_turns(Dir)] ++
        [{get_neigbour(Pos, Dir), Dir, N - 1}].

move1({Dist, PDN}, Map) ->
    [
        {Dist + maps:get(NextPos, Map), Next}
     || {NextPos, _, _} = Next <- next(PDN, 3, 1), maps:is_key(NextPos, Map)
    ].

move2({Dist, PDN}, Map) ->
    [
        {Dist + maps:get(NextPos, Map), Next}
     || {NextPos, _, _} = Next <- next(PDN, 10, 4), maps:is_key(NextPos, Map)
    ].

search(States, End, Map, Visited, Move) ->
    {State, Rest} = gb_sets:take_smallest(States),
    {Dist, PDN} = State,
    {Pos, _, _} = PDN,
    case maps:is_key(PDN, Visited) of
        false when Pos == End ->
            Dist;
        false ->
            New = Move(State, Map),
            Next = gb_sets:union(Rest, gb_sets:from_list(New)),
            search(Next, End, Map, Visited#{PDN => Dist}, Move);
        true ->
            search(Rest, End, Map, Visited, Move)
    end.
