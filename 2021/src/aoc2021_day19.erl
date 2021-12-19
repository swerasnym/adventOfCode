-module(aoc2021_day19).

-export([run/2, profile/3]).

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
        [begin
             {Time, Expected} = timer:tc(F),
             Time
         end
         || _ <- lists:seq(1, Times)],
    {Expected, lists:sum(Results) / Times / 1000}.

star1(Data) ->
    Corrected = find_overlaps([{hd(Data), {0, 0, 0}}], tl(Data), []),
    Points = lists:flatten([Ps || {{_H, Ps}, _Off} <- Corrected]),
    length(lists:usort(Points)).

star2(Data) ->
    Corrected = find_overlaps([{hd(Data), {0, 0, 0}}], tl(Data), []),
    Points = [Off || {{_H, _Ps}, Off} <- Corrected],

    lists:max([abs(X1 - X2) + abs(Y1 - Y2) + abs(Z1 - Z2)
               || {X1, Y1, Z1} <- Points, {X2, Y2, Z2} <- Points]).

read(File) ->
    tools:read_blocks(File, fun parse_block/1).

parse_block(String) ->
    [Head, Rest] = string:split(String, "\n"),
    Pos = [list_to_tuple(tools:parse_integers(L, ",")) || L <- tools:parse_lines(Rest)],
    {Head, Pos}.

orient(Positions, Orientation) ->
    [get_orientation(Pos, Orientation) || Pos <- Positions].

get_orientation(Pos, [X, Y, Z]) ->
    {get_dir(X, Pos), get_dir(Y, Pos), get_dir(Z, Pos)}.

get_dir(x, {X, _, _}) ->
    X;
get_dir(nx, {X, _, _}) ->
    -X;
get_dir(y, {_, Y, _}) ->
    Y;
get_dir(ny, {_, Y, _}) ->
    -Y;
get_dir(z, {_, _, Z}) ->
    Z;
get_dir(nz, {_, _, Z}) ->
    -Z.

orientations() ->
    [[x, y, z],
     [x, z, ny],
     [x, ny, nz],
     [x, nz, y],
     [y, z, x],
     [y, x, nz],
     [y, nz, nx],
     [y, nx, z],
     [nx, ny, z],
     [nx, z, y],
     [nx, y, nz],
     [nx, nz, ny],
     [ny, nz, x],
     [ny, x, z],
     [ny, z, nx],
     [ny, nx, nz],
     [nz, x, ny],
     [nz, ny, nx],
     [nz, nx, y],
     [nz, y, x],
     [z, x, y],
     [z, y, nx],
     [z, nx, ny],
     [z, ny, x]].

translate(List, Translation) when is_list(List) ->
    [translate(L, Translation) || L <- List];
translate({X, Y, Z}, {Dx, Dy, Dz}) ->
    {X + Dx, Y + Dy, Z + Dz}.

diff(List, Translation) when is_list(List) ->
    [diff(L, Translation) || L <- List];
diff({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {X1 - X2, Y1 - Y2, Z1 - Z2}.

find_overlaps([], [], Used) ->
    Used;
find_overlaps([{{_Head, P1s}, _Offset} = First | Rest], Missing, Used) ->
    Oao = [{V, orientation_and_offset(P1s, P2s)} || {_H, P2s} = V <- Missing],
    {StillMissing, Found} = lists:partition(fun({_, OO}) -> OO == none end, Oao),

    Correct =
        [begin
             Oriented = orient(Ps, Orientation),
             Translated = translate(Oriented, Offset),
             {{Head, Translated}, Offset}
         end
         || {{Head, Ps}, {Orientation, Offset}} <- Found],
    %% io:format("StillMissing ~p  Found ~n ~p ~n  Correct ~p~n~n", [StillMissing, Found, Correct]),
    %% io:format("Found ~p~n Corr ~p~n", [Found , Correct]),
    find_overlaps(Rest ++ Correct, [V || {V, none} <- StillMissing], [First | Used]).

orientation_and_offset(P1s, P2s) ->
    case lists:filter(fun({_K, V}) -> V /= [] end, oo(P1s, P2s)) of
        [{Orientation, [Offset]}] ->
            {Orientation, Offset};
        [] ->
            none
    end.

oo(P1s, P2s) ->
    [{O, offsets(P1s, orient(P2s, O))} || O <- orientations()].

offsets(P1s, P2s) ->
    maps:keys(
        maps:filter(fun(_K, V) -> V >= 12 end,
                    tools:count([diff(P1, P2) || P1 <- P1s, P2 <- P2s]))).
