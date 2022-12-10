-module(aoc2022_day9).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day9.txt"),
    io:format("S1: ~p ~nS2: ~p ~n", [S1, S2]),
    Res.

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

read(File) ->
    tools:read_format(File, "~c ~d").

star1(Data) ->
    {Hpath, _} = lists:mapfoldl(fun move_head/2, {0, 0}, Data),
    {Tpath, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, lists:flatten(Hpath)),
    length(lists:uniq(Tpath)).

star2(Data) ->
    {Hpath, _} = lists:mapfoldl(fun move_head/2, {0, 0}, Data),
    {Path1, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, lists:flatten(Hpath)),
    {Path2, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path1),
    {Path3, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path2),
    {Path4, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path3),
    {Path5, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path4),
    {Path6, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path5),
    {Path7, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path6),
    {Path8, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path7),
    {Path9, _} = lists:mapfoldl(fun move_tail/2, {0, 0}, Path8),
    length(lists:uniq(Path9)).

move_tail(H = {Hx, Hy}, T = {Tx, Ty}) ->
    Dx = Hx - Tx,
    Dy = Hy - Ty,
    Res = case {abs(Dx), abs(Dy)} of
              {2, 0} ->
                  {Tx + Dx div 2, Ty};
              {0, 2} ->
                  {Tx, Ty + Dy div 2};
              {2, _} ->
                  [P] = [P1 || P1 <- neigbours(H), P2 <- neigboursDiag(T), P1 == P2],
                  P;
              {_, 2} ->
                  [P] = [P1 || P1 <- neigbours(H), P2 <- neigboursDiag(T), P1 == P2],
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

neigbours({X, Y}) ->
    [{X + Dx, Y + Dy} || Dx <- [-1, 0, 1], Dy <- [-1, 0, 1], {Dx, Dy} /= {0, 0}].

neigboursDiag({X, Y}) ->
    [{X + Dx, Y + Dy} || Dx <- [-1, 1], Dy <- [-1, 1]].
