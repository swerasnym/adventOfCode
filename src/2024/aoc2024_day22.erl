-module(aoc2024_day22).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day22_ex.txt", star1, 37327623},
        {"examples/2024/day22_ex2.txt", star2, 23}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 22},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    lists:sum([tools:repeat(2000, fun next/1, D) || D <- Data]).

star2(Data) ->
    Sequences = [sequences(price_delta(D, 2001), #{}) || D <- Data],
    Merge = fun(_, P1, P2) -> P1 + P2 end,
    Totals = lists:foldl(fun(M, Acc) -> maps:merge_with(Merge, M, Acc) end, #{}, Sequences),
    lists:max(maps:values(Totals)).

read(File) ->
    tools:read_integers(File).

mix(S, R) ->
    S bxor R.

prune(R) ->
    tools:mod(R, 16777216).

next(S) ->
    R1 = prune(mix(S, S * 64)),
    R2 = prune(mix(R1, R1 div 32)),
    prune(mix(R2, R2 * 2048)).

price(S) ->
    tools:mod(S, 10).

prices(S, N) ->
    Secrets = lists:foldl(fun(_, [Hd | _] = Acc) -> [next(Hd) | Acc] end, [S], lists:seq(1, N - 1)),
    lists:reverse([price(R) || R <- Secrets]).

deltas([_], Res) ->
    lists:reverse(Res);
deltas([A | [B | _] = Rest], Res) ->
    deltas(Rest, [B - A | Res]).

price_delta(S, N) ->
    Prices = prices(S, N),
    Deltas = deltas(Prices, []),
    lists:zip(tl(Prices), Deltas).

sequences([{_, A} | [{_, B}, {_, C}, {P, D} | _] = Rest], Res) ->
    S = [A, B, C, D],
    case is_map_key(S, Res) of
        true ->
            sequences(Rest, Res);
        false ->
            sequences(Rest, Res#{S => P})
    end;
sequences(_, Res) ->
    Res.
