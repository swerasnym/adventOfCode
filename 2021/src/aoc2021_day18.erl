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
        [begin
             {Time, Expected} = timer:tc(F),
             Time
         end
         || _ <- lists:seq(1, Times)],
    {Expected, lists:sum(Results) / Times / 1000}.

star1(Data) ->
    Sum = lists:foldl(fun add_s/2, [], Data),
    Magnitude = magnitude(Sum),
    {Magnitude, value_r(Sum)}.

star2(Data) ->
    lists:max([magnitude(add_s(S1, S2)) || S1 <- Data, S2 <- Data]).

read(File) ->
    Lines = tools:read_lines(File),
    [lists:map(fun value/1, Line) || Line <- Lines].

magnitude([V]) when is_integer(V) ->
    V;
magnitude(List) ->
    magnitude(magnitude([], List)).

magnitude(Left, []) ->
    lists:reverse(Left);
magnitude(Left, ['[', A, ',', B, ']' | Right]) when is_integer(A), is_integer(B) ->
    lists:reverse(Left, [3 * A + 2 * B | Right]);
magnitude(Left, [C | Right]) ->
    magnitude([C | Left], Right).

value($[) ->
    '[';
value($]) ->
    ']';
value($,) ->
    ',';
value(C) when $0 =< C, C =< $9 ->
    C - $0.

value_r(List) when is_list(List) ->
    lists:flatmap(fun value_r/1, List);
value_r('[') ->
    "[";
value_r(']') ->
    "]";
value_r(',') ->
    ",";
value_r(C) when is_integer(C) ->
    integer_to_list(C).

reduse(Value) ->
    case reduse("", Value, 0) of
        Value ->
            case spl([], Value) of
                Value ->
                    Value;
                OtherSplit ->
                    reduse(OtherSplit)
            end;
        Other ->
            reduse(Other)
    end.

reduse(Left, [], 0) ->
    lists:reverse(Left);
reduse(Left, ['[', A, ',', B, ']' | _] = Right, Depth)
    when Depth >= 4, is_integer(A), is_integer(B) ->
    {L, R} = explode(Left, Right),
    lists:reverse(L, R);
reduse(Left, ['[' | Right], Depth) ->
    reduse(['[' | Left], Right, Depth + 1);
reduse(Left, [']' | Right], Depth) ->
    reduse([']' | Left], Right, Depth - 1);
reduse(Left, [C | Right], Depth) ->
    reduse([C | Left], Right, Depth).

spl(Left, []) ->
    lists:reverse(Left);
spl(Left, [C | Right]) when is_integer(C), C > 9 ->
    Res = lists:reverse(Left, split(C) ++ Right),
    %io:format("S: ~p~n", [value_r(lists:reverse(Left, [C | Right]))]),
    %io:format(" : ~p~n", [value_r(Res)]),
    Res;
spl(Left, [C | Right]) ->
    spl([C | Left], Right).

explode(Left, ['[', A, ',', B, ']' | Right]) ->
    Rl = explode_i(A, Left),
    Rr = explode_i(B, Right),

    Res = {[0] ++ Rl, Rr},
    %io:format("E: ~p~n", [value_r(lists:reverse(Left)) ++
    %% "<" ++
    %% integer_to_list(A) ++
    %% "," ++
    %% integer_to_list(B) ++
    %% ">" ++  value_r(Right)]),
    %io:format(" : ~p~n~n", [value_r(lists:reverse(Rl)) ++ "#" ++ value_r(Rr)]),
    Res.

explode_i(_A, []) ->
    [];
explode_i(A, [C | Stack]) when is_integer(C) ->
    [A + C] ++ Stack;
explode_i(A, [C | Stack]) ->
    [C | explode_i(A, Stack)].

split(A) when A < 9 ->
    [A];
split(Sum) ->
    An = Sum div 2,
    Bn = Sum - An,
    ['[', An, ',', Bn, ']'].

add_s(Sn1, []) ->
    Sn1;
add_s(Sn2, Sn1) ->
    Res = reduse(['['] ++ Sn1 ++ [','] ++ Sn2 ++ [']']),
    % io:format("  ~p~n+ ~p~n= ~p~n~n", [value_r(Sn1), value_r(Sn2), value_r(Res)]),
    Res.
