-module(aoc2021_day18).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 18},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Sum = lists:foldl(fun add/2, [], Data),
    io:format("~p", [Sum]),
    magnitude(Sum).

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
