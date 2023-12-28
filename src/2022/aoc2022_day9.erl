-module(aoc2022_day9).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 9}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    tools:read_multiple_formats(File, "~c ~d").

star1(Data) ->
    {HPath, _} = lists:mapfoldl(fun move_head/2, {0, 0}, Data),
    {TPath, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, lists:flatten(HPath)),
    length(lists:uniq(TPath)).

star2(Data) ->
    {HPath, _} = lists:mapfoldl(fun move_head/2, {0, 0}, Data),
    {Path1, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, lists:flatten(HPath)),
    {Path2, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path1),
    {Path3, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path2),
    {Path4, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path3),
    {Path5, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path4),
    {Path6, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path5),
    {Path7, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path6),
    {Path8, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path7),
    {Path9, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path8),
    length(lists:uniq(Path9)).

move_tail({Hx, Hy} = H, {Tx, Ty} = T) ->
    Dx = Hx - Tx,
    Dy = Hy - Ty,
    Res =
        case {abs(Dx), abs(Dy)} of
            {2, 0} ->
                {Tx + Dx div 2, Ty};
            {0, 2} ->
                {Tx, Ty + Dy div 2};
            {2, _} ->
                [P] = [P1 || P1 <- neighbours(H), P2 <- neighbours_diag(T), P1 == P2],
                P;
            {_, 2} ->
                [P] = [P1 || P1 <- neighbours(H), P2 <- neighbours_diag(T), P1 == P2],
                P;
            _ ->
                T
        end,
    {Res, Res}.

move_head(["U", U], {X, Y}) ->
    {[{X, Y - Dy} || Dy <- lists:seq(1, U, 1)], {X, Y - U}};
move_head(["D", D], {X, Y}) ->
    {[{X, Y + Dy} || Dy <- lists:seq(1, D, 1)], {X, Y + D}};
move_head(["R", R], {X, Y}) ->
    {[{X + Dx, Y} || Dx <- lists:seq(1, R, 1)], {X + R, Y}};
move_head(["L", L], {X, Y}) ->
    {[{X - Dx, Y} || Dx <- lists:seq(1, L, 1)], {X - L, Y}}.

neighbours({X, Y}) ->
    [{X + Dx, Y + Dy} || Dx <- [-1, 0, 1], Dy <- [-1, 0, 1], {Dx, Dy} /= {0, 0}].

neighbours_diag({X, Y}) ->
    [{X + Dx, Y + Dy} || Dx <- [-1, 1], Dy <- [-1, 1]].
