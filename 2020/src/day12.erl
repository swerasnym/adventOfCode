-module(day12).

-export([run/2]).

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

star1(Data) ->
    Start = {0, 0, 90},

    {X, Y, _H} = lists:foldl(fun(Change, Pos) -> take_action(Pos, Change) end, Start, Data),
    abs(X) + abs(Y).

star2(Data) ->
    Start = {{0, 0}, {1, 10}},

    {{X, Y}, _Wp} =
        lists:foldl(fun(Change, Pos) -> take_action2(Pos, Change) end, Start, Data),
    abs(X) + abs(Y).

read(File) ->
    [action(Dir, Amount) || [Dir, Amount] <- tools:read_format(File, "~c~d")].

action("N", Amount) ->
    {Amount, 0, 0};
action("S", Amount) ->
    {-Amount, 0, 0};
action("E", Amount) ->
    {0, Amount, 0};
action("W", Amount) ->
    {0, -Amount, 0};
action("R", Amount) ->
    {0, 0, Amount};
action("L", Amount) ->
    {0, 0, 360 - Amount};
action("F", Amount) ->
    {move, Amount}.

take_action({X, Y, 0}, {move, Amount}) ->
    {X + Amount, Y, 0};
take_action({X, Y, 90}, {move, Amount}) ->
    {X, Y + Amount, 90};
take_action({X, Y, 180}, {move, Amount}) ->
    {X - Amount, Y, 180};
take_action({X, Y, 270}, {move, Amount}) ->
    {X, Y - Amount, 270};
take_action({X, Y, H}, {Dx, Dy, Dh}) ->
    {X + Dx, Y + Dy, (H + Dh) rem 360}.

take_action2({{Sx, Sy}, {Wx, Wy} = Wp}, {move, N}) ->
    {{Sx + N * Wx, Sy + N * Wy}, Wp};
take_action2({Ship, {Wx, Wy}}, {0, 0, 0}) ->
    {Ship, {Wx, Wy}};
take_action2({Ship, {Wx, Wy}}, {0, 0, 180}) ->
    {Ship, {-Wx, -Wy}};
take_action2({Ship, {Wx, Wy}}, {0, 0, 90}) ->
    {Ship, {-Wy, Wx}};
take_action2({Ship, {Wx, Wy}}, {0, 0, 270}) ->
    {Ship, {Wy, -Wx}};
take_action2({Ship, {Wx, Wy}}, {Dx, Dy, 0}) ->
    {Ship, {Wx + Dx, Wy + Dy}}.
