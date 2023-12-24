-module(aoc2023_day24).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

info() ->
    Examples = [
        {"2023/data/day24_ex.txt", {star1, {7.0, 27.0}}, 2},
        {"2023/data/day24_ex.txt", star2, 47}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 24},
        examples => Examples
    }).

-define(LIMITS, {200000000000000.0, 400000000000000.0}).
run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Hails) ->
    star1(Hails, ?LIMITS).

star1(Hails, {Min, Max}) ->
    Coll = [colide_xy(H1, H2) || H1 <- Hails, H2 <- Hails, H2 < H1],
    Inside = [{X, Y} || {_, {X, Y}} <- Coll, Min =< X, X =< Max, Min =< Y, Y =< Max],
    length(Inside).

star2([{P1, V1} = H1, {P2, V2} = H2, H3 | _]) ->
    %% Oringinally used a Computer Algebra sytem to solve:
    %%
    %% X + V*t_1 = X_1 + V_1 * t_1
    %% X + V*t_2 = X_2 + V_2 * t_2
    %% X + V*t_3 = X_3 + V_3 * t_3

    {Ca, DA} = get_plane(H1, H2),
    {Cb, DB} = get_plane(H1, H3),
    {Cc, DC} = get_plane(H2, H3),

    W0 = add([
        mul(DA, cross(Cb, Cc)),
        mul(DB, cross(Cc, Ca)),
        mul(DC, cross(Ca, Cb))
    ]),

    T = dot(Ca, cross(Cb, Cc)),
    W = idiv(W0, T),

    W1 = sub(V1, W),
    W2 = sub(V2, W),

    WW = cross(W1, W2),

    E = dot(WW, cross(P2, W2)),
    F = dot(WW, cross(P1, W1)),
    G = dot(P1, WW),
    S = dot(WW, WW),

    Rock = add([
        mul(E, W1),
        mul(-F, W2),
        mul(G, WW)
    ]),

    sum(idiv(Rock, S)).

read(File) ->
    tools:group(
        2, tools:group(3, lists:flatten(tools:read_format(File, "~d, ~d, ~d @ ~d, ~d, ~d")))
    ).

get_plane({P1, V1}, {P2, V2}) ->
    Dp = sub(P1, P2),
    Dv = sub(V1, V2),
    Cv = cross(V1, V2),
    {cross(Dp, Dv), dot(Dp, Cv)}.

colide_xy(H1, H2) ->
    collide({1, 2}, H1, H2).

collide({N, M}, H1, H2) ->
    {Pn1, Vn1} = get_pair(N, H1),
    {Pn2, Vn2} = get_pair(N, H2),

    case find_crossing(tokxpm({N, M}, H1), tokxpm({N, M}, H2)) of
        {X, Y} ->
            T1 = (X - Pn1) / Vn1,
            T2 = (X - Pn2) / Vn2,
            T = min(T1, T2),
            case T >= 0.0 of
                true ->
                    {T, {X, Y}};
                false ->
                    to_early
            end;
        same ->
            {same};
        none ->
            none
    end.

find_crossing({K1, M1} = A, {K2, M2} = B) ->
    Dk = (K2 - K1),
    case abs(Dk) > 0.0 of
        true ->
            X = (M1 - M2) / Dk,
            Y = K1 * X + M1,
            {X, Y};
        false when A == B ->
            same;
        false ->
            none
    end.

tokpm({P1, D1}, {P2, D2}) ->
    K = D2 / D1,
    M = P2 - K * P1,
    {K, M}.

tokxpm({M, N}, Hail) ->
    tokpm(get_pair(M, Hail), get_pair(N, Hail)).

get_pair(N, {P, V}) ->
    {element(N, P), element(N, V)}.

sum(A) when is_tuple(A) ->
    lists:sum(erlang:tuple_to_list(A)).
add([A]) ->
    A;
add([A, B | Rest]) ->
    add([add(A, B) | Rest]).

add(A, B) when is_tuple(A), is_tuple(B), tuple_size(A) == tuple_size(B) ->
    erlang:list_to_tuple([element(N, A) + element(N, B) || N <- lists:seq(1, tuple_size(A))]).
sub(A, B) when is_tuple(A), is_tuple(B), tuple_size(A) == tuple_size(B) ->
    erlang:list_to_tuple([element(N, A) - element(N, B) || N <- lists:seq(1, tuple_size(A))]).
dot(A, B) when is_tuple(A), is_tuple(B), tuple_size(A) == tuple_size(B) ->
    lists:sum([element(N, A) * element(N, B) || N <- lists:seq(1, tuple_size(A))]).

mul(K, V) when is_number(K), is_tuple(V) ->
    erlang:list_to_tuple([K * element(N, V) || N <- lists:seq(1, tuple_size(V))]).

idiv(V, K) when is_integer(K), is_tuple(V) ->
    erlang:list_to_tuple([element(N, V) div K || N <- lists:seq(1, tuple_size(V))]).

cross({X1, Y1, Z1}, {X2, Y2, Z2}) ->
    {
        Y1 * Z2 - Z1 * Y2,
        Z1 * X2 - X1 * Z2,
        X1 * Y2 - Y1 * X2
    }.
