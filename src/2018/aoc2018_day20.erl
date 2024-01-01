-module(aoc2018_day20).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2018/day20_ex4.txt", star1, 23},
        {"examples/2018/day20_ex4.txt", star2, 0}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 20},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Expression) ->
    {_, Map} = walk([{0, 0}], Expression, #{}),
    print(Map),
    Visited = bfs([{{0, 0}, 0}], Map, #{}),
    lists:max(maps:values(Visited)).

star2(Expression) ->
    {_, Map} = walk([{0, 0}], Expression, #{}),
    Visited = bfs([{{0, 0}, 0}], Map, #{}),
    length([V || V <- maps:values(Visited), V >= 1000]).

read(File) ->
    Expr0 = tools:replace(
        tools:read_string(File),
        #{
            $^ => "[{\"",
            $( => "\"},[[{\"",
            $| => "\"}],[{\"*",
            $) => "\"}]],{\"",
            $$ => "\"}]."
        }
    ),

    Expr1 = string:replace(Expr0, ",{\"\"}", "", all),
    Expr2 = string:replace(Expr1, "*", "", all),
    tools:as_term(lists:flatten(Expr2)).

walk(Positions, [], Map) ->
    {Positions, Map};
% walk(Positions, {S}, Map) ->
%     walk_string(Positions, S, Map);
walk(Positions, [{S} | Rest], Map) ->
    {NewPos, NewMap} = walk_string(Positions, S, Map),
    walk(NewPos, Rest, NewMap);
walk(Positions, [Expressions | Rest], Map) ->
    {Plist, NewMap} = lists:mapfoldl(fun(E, M) -> walk(Positions, E, M) end, Map, Expressions),
    walk(lists:append(Plist), Rest, NewMap).

walk_string(Positions, [], Map) ->
    {Positions, Map};
walk_string(Positions, [Dir | Rest], Map) ->
    NewPositions0 = [move2(P, Dir) || P <- Positions],
    NewPositions = lists:usort(NewPositions0),
    Map1 = lists:foldl(fun(Pos, M) -> add_door(Pos, Dir, M) end, Map, Positions),
    ODir = opposite(Dir),
    NewMap = lists:foldl(fun(Pos, M) -> add_door(Pos, ODir, M) end, Map1, NewPositions),
    walk_string(NewPositions, Rest, NewMap).

move1(Pos, $N) -> add(Pos, {0, -1});
move1(Pos, $S) -> add(Pos, {0, 1});
move1(Pos, $E) -> add(Pos, {1, 0});
move1(Pos, $W) -> add(Pos, {-1, 0}).

move2(Pos, $N) -> add(Pos, {0, -2});
move2(Pos, $S) -> add(Pos, {0, 2});
move2(Pos, $E) -> add(Pos, {2, 0});
move2(Pos, $W) -> add(Pos, {-2, 0}).

opposite($N) -> $S;
opposite($S) -> $N;
opposite($E) -> $W;
opposite($W) -> $E.

add_door(Pos, Dir, Map) when is_map_key(Pos, Map) ->
    Current = maps:get(Pos, Map),
    case lists:member(Dir, Current) of
        true ->
            Map;
        false ->
            Map#{Pos := [Dir | Current]}
    end;
add_door(Pos, Dir, Map) ->
    Map#{Pos => [Dir]}.

print(Map) ->
    {{Xmin, Xmax}, {Ymin, Ymax}} = tools:min_max_grid(Map),
    Walls =
        #{{X, Y} => $█ || X <- lists:seq(Xmin - 1, Xmax + 1), Y <- lists:seq(Ymin - 1, Ymax + 1)},
    Rooms0 = #{K => $\s || K <- maps:keys(Map)},
    Rooms = Rooms0#{{0, 0} => $X},
    Doors = lists:flatten([[move1(P, D) || D <- Dirs] || P := Dirs <- Map]),

    tools:print_grid(maps:merge(Walls, maps:merge(Rooms, maps:from_keys(Doors, $\s)))).

add({X1, Y1}, {X2, Y2}) ->
    {X1 + X2, Y1 + Y2}.

bfs([], _Map, Visited) ->
    Visited;
bfs([{P, _} | Rest], Map, Visited) when is_map_key(P, Visited) ->
    bfs(Rest, Map, Visited);
bfs([{Pos, Steps} | Rest], Map, Visited) ->
    Next = [{move2(Pos, D), Steps + 1} || D <- maps:get(Pos, Map)],
    bfs(Rest ++ Next, Map, Visited#{Pos => Steps}).
