-module(aoc2021_day20).

-export([run/2, profile/3, eprof/2]).

-define(DARK, $0).
-define(LIGHT, $1).

run(Star, File) ->
    Data = read(File),
    case Star of
        star1 ->
            star1(Data);
        star2 ->
            star2(Data);
        _ ->
            Star1 = star1(Data),
            Star2 = star2(Data),
            {Star1, Star2}
    end.

profile(Star, File, Times) ->
    Data = read(File),
    case Star of
        star1 ->
            profile(fun() -> star1(Data) end, Times);
        star2 ->
            profile(fun() -> star2(Data) end, Times);
        _ ->
            Star1 = profile(fun() -> star1(Data) end, Times),
            Star2 = profile(fun() -> star2(Data) end, Times),
            {Star1, Star2}
    end.

profile(F, Times) ->
    Expected = F(),
    Results =
        [
            begin
                {Time, Expected} = timer:tc(F),
                Time
            end
         || _ <- lists:seq(1, Times)
        ],
    {Expected, lists:sum(Results) / Times / 1000}.

eprof(Star, File) ->
    eprof:start(),
    eprof:start_profiling([self()]),
    Result = run(Star, File),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop(),
    Result.

star1(Grid) ->
    {Grid1, Out1} = enhance(Grid, ?DARK),

    {Grid2, _Out2} = enhance(Grid1, Out1),
    print(Grid2),
    tools:count(?LIGHT, Grid2).

star2(Grid) ->
    Grid50 = enhance(Grid, ?DARK, 50),
    tools:count(?LIGHT, Grid50).

read(File) ->
    erase(),
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
