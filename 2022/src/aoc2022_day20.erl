-module(aoc2022_day20).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day20.txt"),
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
    tools:read_integers(File).

star1(Data) ->
    {_, Mixed} =
        lists:unzip(mix(lists:seq(1, length(Data)), lists:enumerate(Data), length(Data))),

    {Before, [0 | After]} = lists:splitwith(fun(X) -> X /= 0 end, Mixed),

    Relative = After ++ Before ++ [0],

    27726 =
        lists:nth(1000 rem length(Data), Relative)
        + lists:nth(2000 rem length(Data), Relative)
        + lists:nth(3000 rem length(Data), Relative).

star2(Data) ->
    Mix0 = [D * 811589153 || D <- Data],

    Mixed1 = mix(lists:seq(1, length(Data)), lists:enumerate(Mix0), length(Data)),
    Mixed2 = mix(lists:seq(1, length(Data)), Mixed1, length(Data)),
    Mixed3 = mix(lists:seq(1, length(Data)), Mixed2, length(Data)),
    Mixed4 = mix(lists:seq(1, length(Data)), Mixed3, length(Data)),
    Mixed5 = mix(lists:seq(1, length(Data)), Mixed4, length(Data)),
    Mixed6 = mix(lists:seq(1, length(Data)), Mixed5, length(Data)),
    Mixed7 = mix(lists:seq(1, length(Data)), Mixed6, length(Data)),
    Mixed8 = mix(lists:seq(1, length(Data)), Mixed7, length(Data)),
    Mixed9 = mix(lists:seq(1, length(Data)), Mixed8, length(Data)),
    {_, Mixed} = lists:unzip(mix(lists:seq(1, length(Data)), Mixed9, length(Data))),

    {Before, [0 | After]} = lists:splitwith(fun(X) -> X /= 0 end, Mixed),

    Relative = After ++ Before ++ [0],

    4275451658004 =
        lists:nth(1000 rem length(Data), Relative)
        + lists:nth(2000 rem length(Data), Relative)
        + lists:nth(3000 rem length(Data), Relative).

mix([], Mixed, _) ->
    Mixed;
mix([Idx | Rest], Mixed, Length) ->
    {Before, [Elem = {Idx, ToMove} | After]} =
        lists:splitwith(fun({X, _}) -> X /= Idx end, Mixed),

    MixedOut =
        case ToMove rem (Length - 1) of
            N when N >= 0 ->
                {B, A} = lists:split(N, After ++ Before),
                B ++ [Elem | A];
            N when N < 0 ->
                {A, B} = lists:split(-N, lists:reverse(After ++ Before)),
                lists:reverse(B) ++ [Elem | lists:reverse(A)]
        end,

    mix(Rest, MixedOut, Length).
