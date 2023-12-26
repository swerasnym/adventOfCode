-module(aoc2021_day20).
-behaviour(aoc_solution).

-define(DARK, $0).
-define(LIGHT, $1).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 20},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Grid) ->
    {Grid1, Out1} = enhance(Grid, ?DARK),

    {Grid2, _Out2} = enhance(Grid1, Out1),
    print(Grid2),
    tools:count(?LIGHT, Grid2).

star2(Grid) ->
    Grid50 = enhance(Grid, ?DARK, 50),
    tools:count(?LIGHT, Grid50).

read(File) ->
    erlang:erase(),
    Replacements = #{$# => ?LIGHT, $. => ?DARK},

    [LineStr, GridBlock] = tools:read_blocks(File),
    Grid = tools:parse_grid(GridBlock, Replacements),
    Line = tools:replace(LineStr, Replacements),
    ok = build_nth(Line, 0),
    Grid#{min => {0, 0}}.

print(Grid) ->
    tools:print_grid(tools:replace(Grid, #{?LIGHT => $█, ?DARK => $\s})).

index({X, Y} = Pos, Grid, Outside) ->
    Sequence =
        [maps:get({X + Dx, Y + Dy}, Grid, Outside) || Dy <- [-1, 0, 1], Dx <- [-1, 0, 1]],
    Index = list_to_integer(Sequence, 2),
    {Pos, nth(Index)}.

enhance(Grid, _Outside, 0) ->
    Grid;
enhance(Grid, Outside, N) ->
    {Grid1, Out1} = enhance(Grid, Outside),
    enhance(Grid1, Out1, N - 1).

enhance(#{min := {Xmin, Ymin}, max := {Xmax, Ymax}} = Grid, Outside) ->
    Processed =
        [
            index({X, Y}, Grid, Outside)
         || X <- lists:seq(Xmin - 1, Xmax + 1), Y <- lists:seq(Ymin - 1, Ymax + 1)
        ],
    Out = maps:from_list(
        Processed ++
            [{min, {Xmin - 1, Ymin - 1}}, {max, {Xmax + 1, Ymax + 1}}]
    ),
    Os =
        case Outside of
            ?DARK ->
                nth(0);
            ?LIGHT ->
                nth(511)
        end,
    {Out, Os}.

build_nth([], _) ->
    ok;
build_nth([Hd | Rest], N) ->
    erlang:put(N, Hd),
    build_nth(Rest, N + 1).

nth(N) ->
    erlang:get(N).
