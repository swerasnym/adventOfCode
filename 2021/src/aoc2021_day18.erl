-module(aoc2021_day18).

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
        [
            begin
                {Time, Expected} = timer:tc(F),
                Time
            end
         || _ <- lists:seq(1, Times)
        ],
    {Expected, lists:sum(Results) / Times / 1000}.

star1(Data) ->
    Sum = lists:foldl(fun add/2, [], Data),
    Magnitude = magnitude(Sum),
    {Magnitude, Sum}.

star2(Data) ->
    lists:max([magnitude(add(S1, S2)) || S1 <- Data, S2 <- Data]).

read(File) ->
    tools:read_lines(File, fun tools:as_term/1).

magnitude(V) when is_integer(V) ->
    V;
magnitude([L, R]) ->
    3 * magnitude(L) + 2 * magnitude(R).

add(Sn1, []) ->
    Sn1;
add(Sn2, Sn1) ->
    reduse([Sn1, Sn2]).

reduse(Value) ->
    case explode(Value, 0) of
        {ok, ok, Value} ->
            case split(Value) of
                {done, Split} ->
                    reduse(Split);
                Value ->
                    Value
            end;
        {_, _, Other} ->
            reduse(Other)
    end.

explode([A, B], Depth) when Depth >= 4, is_integer(A), is_integer(B) ->
    {A, B, 0};
explode([A, B], _Depth) when is_integer(A), is_integer(B) ->
    {ok, ok, [A, B]};
explode([Left, Right], Depth) when is_list(Left), is_list(Right) ->
    case explode(Left, Depth + 1) of
        {ok, ok, Left} ->
            {L, R, Result} = explode(Right, Depth + 1),
            {ok, R, [push_left(L, Left), Result]};
        {ok, ok, Result} ->
            {ok, ok, [Result, Right]};
        {L, R, Vl} ->
            {L, ok, [Vl, push_right(R, Right)]}
    end;
explode([A, Right], Depth) when is_integer(A), is_list(Right) ->
    case explode(Right, Depth + 1) of
        {ok, R, Result} ->
            {ok, R, [A, Result]};
        {L, R, Result} ->
            {ok, R, [A + L, Result]}
    end;
explode([Left, B], Depth) when is_integer(B), is_list(Left) ->
    case explode(Left, Depth + 1) of
        {L, ok, Result} ->
            {L, ok, [Result, B]};
        {L, R, Result} ->
            {L, ok, [Result, R + B]}
    end.

split(A) when is_integer(A), A > 9 ->
    An = A div 2,
    Bn = A - An,
    {done, [An, Bn]};
split(A) when is_integer(A) ->
    A;
split([Left, Right]) ->
    case split(Left) of
        {done, Result} ->
            {done, [Result, Right]};
        Left ->
            case split(Right) of
                {done, Result} ->
                    {done, [Left, Result]};
                Right ->
                    [Left, Right]
            end
    end.

push_right(ok, Right) ->
    Right;
push_right(R, [L, Right]) when is_integer(L) ->
    [R + L, Right];
push_right(R, [Left, Right]) ->
    [push_right(R, Left), Right].

push_left(ok, Left) ->
    Left;
push_left(L, [Left, R]) when is_integer(R) ->
    [Left, L + R];
push_left(L, [Left, Right]) ->
    [Left, push_left(L, Right)].
