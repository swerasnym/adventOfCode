-module(aoc2018_day22).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, {510, {10, 10}}}, star1, 114},
        {{data, {510, {10, 10}}}, star2, 45}
        %{"examples/2018/day22_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 22},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Depth, {Tx, Ty} = Target}) ->
    erlang:erase(),
    erlang:put({gi, Depth, Target}, 0),

    Map =
        #{
            {X, Y} => symbol(risk_level(Depth, {X, Y}))
         || X <- lists:seq(0, Tx + 5), Y <- lists:seq(0, Ty + 5)
        },
    tools:print_grid(Map#{{0, 0} => $M, Target => $T}),

    lists:sum([risk_level(Depth, {X, Y}) || X <- lists:seq(0, Tx), Y <- lists:seq(0, Ty)]).

star2({Depth, Target}) ->
    erlang:erase(),
    erlang:put({gi, Depth, Target}, 0),
    rocky = type(Depth, Target),
    Start = gb_sets:from_list([{0, 0, {{0, 0}, torch}, start}]),
    search(Start, {Target, torch}, #{}, Depth).

read(File) ->
    [DepthS, TargetS] = tools:read_lines(File),
    [Depth] = tools:parse_format(DepthS, "depth: ~d"),
    [X, Y] = tools:parse_format(TargetS, "target: ~d,~d"),
    {Depth, {X, Y}}.

geologic_index(_Depth, {0, 0}) ->
    0;
geologic_index(_Depth, {X, 0}) ->
    X * 16807;
geologic_index(_Depth, {0, Y}) ->
    Y * 48271;
geologic_index(Depth, {X, Y} = P) ->
    case erlang:get({gi, Depth, P}) of
        undefined ->
            Gi = erosion_level(Depth, {X - 1, Y}) * erosion_level(Depth, {X, Y - 1}),
            erlang:put({gi, Depth, P}, Gi),
            Gi;
        Gi ->
            Gi
    end.

erosion_level(Depth, Pos) ->
    (geologic_index(Depth, Pos) + Depth) rem 20183.

risk_level(Depth, Pos) ->
    erosion_level(Depth, Pos) rem 3.

symbol(0) -> $.;
symbol(1) -> $=;
symbol(2) -> $|.

type(Depth, Pos) ->
    case risk_level(Depth, Pos) of
        0 -> rocky;
        1 -> wet;
        2 -> narrow
    end.

search(States, {EndPos, _} = End, Visited, Depth) ->
    {State, Rest} = gb_sets:take_smallest(States),
    {_Est, Dist, PE, From} = State,
    case maps:is_key(PE, Visited) of
        false when PE == End ->
            % io:format("~p~n", [State]),
            Dist;
        false ->
            % io:format("~p~n", [State]),
            New = new(PE, Depth, Dist, EndPos),
            Next = gb_sets:union(Rest, gb_sets:from_list(New)),
            search(Next, End, Visited#{PE => {Dist, From}}, Depth);
        true ->
            search(Rest, End, Visited, Depth)
    end.

equipment() -> [climbing, torch, neither].

allowed(rocky, climbing) -> true;
allowed(rocky, torch) -> true;
allowed(rocky, neither) -> false;
allowed(wet, climbing) -> true;
allowed(wet, torch) -> false;
allowed(wet, neither) -> true;
allowed(narrow, climbing) -> false;
allowed(narrow, torch) -> true;
allowed(narrow, neither) -> true.

allowed(_, {X, Y}, _) when X < 0 orelse Y < 0 ->
    false;
allowed(Depth, Pos, Equipment) ->
    allowed(type(Depth, Pos), Equipment).

neighbours({X, Y}) ->
    [{X + Dx, Y + Dy} || {Dx, Dy} <- [{1, 0}, {-1, 0}, {0, 1}, {0, -1}]].

new({Pos, Equipment} = PE, Depth, Dist, EndPos) ->
    Move = [
        {Dist + dist(N, EndPos) + 1, Dist + 1, {N, Equipment}, PE}
     || N <- neighbours(Pos), allowed(Depth, N, Equipment)
    ],
    Change = [
        {Dist + dist(Pos, EndPos) + 7, Dist + 7, {Pos, E}, PE}
     || E <- equipment(), allowed(Depth, Pos, E)
    ],
    Move ++ Change.

dist({X, Y}, {Ex, Ey}) ->
    abs(X - Ex) + abs(Y - Ey).
