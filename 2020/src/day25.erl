-module(day25).

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

star1([P1, P2]) ->
    transform2(transform1(0, 1, P1), P2, 1).

star2(_Data) ->
    "Pay Deposit!".

read(File) ->
    tools:read_integers(File).

transform2(0, _S, R) ->
    R;
transform2(N, S, V) ->
    transform2(N - 1, S, V * S rem 20201227).

transform1(N, E, E) ->
    N;
transform1(N, V, E) ->
    transform1(N + 1, V * 7 rem 20201227, E).
