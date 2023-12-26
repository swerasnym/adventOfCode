-module(aoc2022_day22).
-behaviour(aoc_solution).
-hank([{unnecessary_function_arguments, [{wrap2, 3}]}]).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 22}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    [MapBoard, Path0] = tools:read_blocks(File),
    Map = tools:parse_grid(MapBoard),
    Path1 = lists:flatten(tools:replace(Path0, #{$L => " L ", $R => " R "})),
    Path = [parse(E) || E <- string:split(Path1, " ", all)],
    {Map, Path}.

star1({Map, Path}) ->
    {F, {Cm1, Rm1}} = traverse(Path, wrap({0, 0}, east, Map), {Map, fun wrap/3}),
    1000 * (Rm1 + 1) + 4 * (Cm1 + 1) + facing(F).

star2({Map, Path}) ->
    {F, {Cm1, Rm1}} = traverse(Path, wrap({0, 0}, east, Map), {Map, fun wrap2/3}),
    1000 * (Rm1 + 1) + 4 * (Cm1 + 1) + facing(F).

traverse([], DirPos, _MW) ->
    DirPos;
traverse([LR | Rest], {Dir, Pos}, MW) when is_atom(LR) ->
    traverse(Rest, {turn(Dir, LR), Pos}, MW);
traverse([0 | Rest], DirPos, MW) ->
    traverse(Rest, DirPos, MW);
traverse([N | Rest], {Dir, Pos} = DirPos, {Map, Wrapper} = MW) ->
    Next = move(Pos, Dir),
    case maps:get(Next, Map, $\s) of
        $. ->
            traverse([N - 1 | Rest], {Dir, Next}, MW);
        $# ->
            traverse(Rest, DirPos, MW);
        $\s ->
            Wrapped = {_, NextW} = Wrapper(Pos, Dir, Map),
            %%    io:format("~p to ~p~n", [DirPos, Wrapped]),
            case maps:get(NextW, Map) of
                $. ->
                    traverse([N - 1 | Rest], Wrapped, MW);
                $# ->
                    traverse(Rest, DirPos, MW)
            end
    end.

parse("L") ->
    left;
parse("R") ->
    right;
parse(N) ->
    list_to_integer(N).

turn(north, left) ->
    west;
turn(north, right) ->
    east;
turn(west, left) ->
    south;
turn(west, right) ->
    north;
turn(south, left) ->
    east;
turn(south, right) ->
    west;
turn(east, left) ->
    north;
turn(east, right) ->
    south.

move({X, Y}, north) ->
    {X, Y - 1};
move({X, Y}, south) ->
    {X, Y + 1};
move({X, Y}, east) ->
    {X + 1, Y};
move({X, Y}, west) ->
    {X - 1, Y}.

wrap({X, _}, north, Map) ->
    {north, lists:max([P || P = {Xk, _} <- maps:keys(Map), Xk == X, maps:get(P, Map) /= $\s])};
wrap({X, _}, south, Map) ->
    {south, lists:min([P || P = {Xk, _} <- maps:keys(Map), Xk == X, maps:get(P, Map) /= $\s])};
wrap({_, Y}, east, Map) ->
    {east, lists:min([P || P = {_, Yk} <- maps:keys(Map), Yk == Y, maps:get(P, Map) /= $\s])};
wrap({_, Y}, west, Map) ->
    {west, lists:max([P || P = {_, Yk} <- maps:keys(Map), Yk == Y, maps:get(P, Map) /= $\s])}.

wrap2(Pos, Dir, _Map) ->
    wrap2(Pos, Dir).

wrap2({X, 100}, north) ->
    {east, {50, X + 50}};
wrap2({X, 0}, north) when X < 100 ->
    {east, {0, X + 100}};
wrap2({X, 0}, north) when X < 150 ->
    {north, {X - 100, 199}};
wrap2({149, Y}, east) ->
    {west, {99, 149 - Y}};
wrap2({99, Y}, east) when Y < 100 ->
    {north, {50 + Y, 49}};
wrap2({99, Y}, east) when Y < 150 ->
    {west, {149, 149 - Y}};
wrap2({49, Y}, east) when Y < 200 ->
    {north, {Y - 100, 149}};
wrap2({X, 199}, south) ->
    {south, {X + 100, 0}};
wrap2({X, 149}, south) ->
    {west, {49, 100 + X}};
wrap2({X, 49}, south) ->
    {west, {99, X - 50}};
wrap2({0, Y}, west) when Y < 150 ->
    {east, {50, 149 - Y}};
wrap2({0, Y}, west) when Y < 200 ->
    {south, {Y - 100, 0}};
wrap2({50, Y}, west) when Y < 50 ->
    {east, {0, 149 - Y}};
wrap2({50, Y}, west) when Y < 100 ->
    {south, {Y - 50, 100}}.

facing(east) ->
    0;
facing(south) ->
    1;
facing(west) ->
    2;
facing(north) ->
    3.
