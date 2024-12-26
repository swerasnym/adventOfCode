-module(aoc2016_day22).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day22_ex.txt", star1, 7},
        {"examples/2016/day22_ex.txt", star2, 7}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 22},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Grid) ->
    % io:format("~p~n", [moves(Grid)]),
    length(moves(Grid)).

star2(Grid) ->
    {Empty, Map} = empty_map(Grid),
    tools:print_grid(Map),
    X = lists:max([X || {X, 0} <- maps:keys(Grid)]),
    Goal = {X, 0},
    calculate_cost(Goal, Empty, Map, 0).

read(File) ->
    [_, _ | Nodes] = tools:read_lines(File),
    maps:from_list([parse_df(N) || N <- Nodes]).

parse_df(Node) ->
    [X, Y, Size, Used, Avail, _] = tools:parse_format(Node, "/dev/grid/node-x~d-y~d~dT~dT~dT~d%"),
    {{X, Y}, {Size, Used, Avail}}.
can_move(From, To, Grid) ->
    F = maps:get(From, Grid),
    T = maps:get(To, Grid),
    can_move(F, T).

can_move({_, 0, _}, _) -> false;
can_move({_, Used, _}, {_, _, Avail}) -> Used =< Avail.

used({_, U, _}) -> U.
disk_size({S, _, _}) -> S.

moves(Grid) ->
    [{A, B} || A := UsageA <- Grid, B := UsageB <- Grid, A /= B, can_move(UsageA, UsageB)].

neighbours(Map) ->
    fun(Pos) ->
        Ns = [aoc_vector:add(Pos, Dp) || Dp <- [{1, 0}, {0, 1}, {0, -1}, {-1, 0}]],
        [N || N <- Ns, maps:get(N, Map, $#) /= $#]
    end.

calculate_cost({0, 0}, _, _, Cost) ->
    Cost;
calculate_cost({X, Y} = Goal, Empty, Map, Cost) ->
    NewGoal = {X - 1, Y},
    {Dist, _, _} = aoc_graph:bfs(Empty, NewGoal, neighbours(Map#{Goal => $#})),
    calculate_cost(NewGoal, Goal, Map, Cost + Dist + 1).

empty_map(Grid) ->
    % Theory: there exists empty disk in the grid that is the only one we can move to in star1.
    [E] = lists:usort([To || {_, To} <- moves(Grid)]),
    0 = used(maps:get(E, Grid)),

    % Theory the data that we could move into E, would also fit on all disks that could be moved to E
    {Movable, UnMovable} = lists:partition(fun(F) -> can_move(F, E, Grid) end, maps:keys(Grid)),
    MaxUsed = lists:max([used(maps:get(M, Grid)) || M <- Movable]),
    MinSize = lists:min([disk_size(maps:get(M, Grid)) || M <- Movable]),
    true = MaxUsed =< MinSize,

    % If the theories are true return a map and the empty disc.
    Map1 = maps:from_keys(Movable, $.),
    Map2 = maps:from_keys(UnMovable, $#),
    Map3 = maps:merge(Map1, Map2),
    {E, Map3#{E => $E}}.
