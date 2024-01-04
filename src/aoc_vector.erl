-module(aoc_vector).
-export([cross/2, dot/2]).
-export([add/2, sub/2, mul/2, div_f/2, div_i/2]).
-export([sum/1, lin_comb/1]).
-export([three_plane_intersection/3, three_plane_intersection_i/3]).
-export([manhattan/2]).

lin_comb(KVs) ->
    sum([mul(K, V) || {K, V} <- KVs]).

sum([U]) ->
    U;
sum([U, V | Rest]) ->
    sum([add(U, V) | Rest]).

sub(K, A) when is_number(K), is_tuple(A) ->
    erlang:list_to_tuple(sub(K, erlang:tuple_to_list(A)));
sub(A, K) when is_number(K), is_tuple(A) ->
    erlang:list_to_tuple(sub(erlang:tuple_to_list(A), K));
sub(A, B) when is_tuple(A), is_tuple(B) ->
    erlang:list_to_tuple(sub(erlang:tuple_to_list(A), erlang:tuple_to_list(B)));
sub(K, V) when is_number(K), is_list(V) ->
    [K - Vi || Vi <- V];
sub(V, K) when is_number(K), is_list(V) ->
    [Vi - K || Vi <- V];
sub(U, V) when is_list(U), is_list(V), length(U) == length(V) ->
    lists:zipwith(fun erlang:'-'/2, U, V).

add(K, A) when is_number(K), is_tuple(A) ->
    erlang:list_to_tuple(add(K, erlang:tuple_to_list(A)));
add(A, K) when is_number(K), is_tuple(A) ->
    erlang:list_to_tuple(add(erlang:tuple_to_list(A), K));
add(A, B) when is_tuple(A), is_tuple(B) ->
    erlang:list_to_tuple(add(erlang:tuple_to_list(A), erlang:tuple_to_list(B)));
add(K, V) when is_number(K), is_list(V) ->
    [K + Vi || Vi <- V];
add(V, K) when is_number(K), is_list(V) ->
    [Vi + K || Vi <- V];
add(U, V) when is_list(U), is_list(V), length(U) == length(V) ->
    lists:zipwith(fun erlang:'+'/2, U, V).

mul(K, A) when is_number(K), is_tuple(A) ->
    erlang:list_to_tuple(mul(K, erlang:tuple_to_list(A)));
mul(A, K) when is_number(K), is_tuple(A) ->
    erlang:list_to_tuple(mul(erlang:tuple_to_list(A), K));
mul(A, B) when is_tuple(A), is_tuple(B) ->
    erlang:list_to_tuple(mul(erlang:tuple_to_list(A), erlang:tuple_to_list(B)));
mul(K, V) when is_number(K), is_list(V) ->
    [K * Vi || Vi <- V];
mul(V, K) when is_number(K), is_list(V) ->
    [Vi * K || Vi <- V];
mul(U, V) when is_list(U), is_list(V), length(U) == length(V) ->
    lists:zipwith(fun erlang:'*'/2, U, V).

div_f(K, A) when is_number(K), is_tuple(A) ->
    erlang:list_to_tuple(div_f(K, erlang:tuple_to_list(A)));
div_f(A, K) when is_number(K), is_tuple(A) ->
    erlang:list_to_tuple(div_f(erlang:tuple_to_list(A), K));
div_f(A, B) when is_tuple(A), is_tuple(B) ->
    erlang:list_to_tuple(div_f(erlang:tuple_to_list(A), erlang:tuple_to_list(B)));
div_f(K, V) when is_number(K), is_list(V) ->
    [K / Vi || Vi <- V];
div_f(V, K) when is_number(K), is_list(V) ->
    [Vi / K || Vi <- V];
div_f(U, V) when is_list(U), is_list(V), length(U) == length(V) ->
    lists:zipwith(fun erlang:'/'/2, U, V).

div_i(K, A) when is_number(K), is_tuple(A) ->
    erlang:list_to_tuple(div_i(K, erlang:tuple_to_list(A)));
div_i(A, K) when is_number(K), is_tuple(A) ->
    erlang:list_to_tuple(div_i(erlang:tuple_to_list(A), K));
div_i(A, B) when is_tuple(A), is_tuple(B) ->
    erlang:list_to_tuple(div_i(erlang:tuple_to_list(A), erlang:tuple_to_list(B)));
div_i(K, V) when is_number(K), is_list(V) ->
    [K div Vi || Vi <- V];
div_i(V, K) when is_number(K), is_list(V) ->
    [Vi div K || Vi <- V];
div_i(U, V) when is_list(U), is_list(V), length(U) == length(V) ->
    lists:zipwith(fun erlang:'div'/2, U, V).

dot(A, B) when is_tuple(A), is_tuple(B) ->
    dot(erlang:tuple_to_list(A), erlang:tuple_to_list(B));
dot(U, V) when is_list(U), is_list(V), length(U) == length(V) ->
    lists:sum(mul(U, V)).

cross(A, B) when is_tuple(A), is_tuple(B) ->
    erlang:list_to_tuple(cross(erlang:tuple_to_list(A), erlang:tuple_to_list(B)));
cross([X1, Y1, Z1], [X2, Y2, Z2]) ->
    [
        Y1 * Z2 - Z1 * Y2,
        Z1 * X2 - X1 * Z2,
        X1 * Y2 - Y1 * X2
    ].

manhattan(A, B) when is_tuple(A), is_tuple(B) ->
    manhattan(erlang:tuple_to_list(A), erlang:tuple_to_list(B));
manhattan(U, V) when is_list(U), is_list(V) ->
    lists:sum([abs(D) || D <- sub(U, V)]).

three_plane_intersection(P1, P2, P3) ->
    three_plane_intersection(pf_(P1), pf_(P2), pf_(P3), fun erlang:'/'/2).

three_plane_intersection_i(P1, P2, P3) ->
    three_plane_intersection(pf_(P1), pf_(P2), pf_(P3), fun erlang:'div'/2).

pf_({{Nx, Ny, Nz}, D}) ->
    [Nx, Ny, Nz, D];
pf_({D, {Nx, Ny, Nz}}) ->
    [Nx, Ny, Nz, D];
pf_(P) ->
    P.

%% @doc X*Nx + Y*Ny + Z*Nz = D
%%
%%
%%
three_plane_intersection(
    [Nx1, Ny1, Nz1, D1],
    [Nx2, Ny2, Nz2, D2],
    [Nx3, Ny3, Nz3, D3],
    Div
) ->
    Nx = [Nx1, Nx2, Nx3],
    Ny = [Ny1, Ny2, Ny3],
    Nz = [Nz1, Nz2, Nz3],
    D = [D1, D2, D3],

    U = cross(Ny, Nz),
    V = cross(Nx, D),

    %% Todo check for value close to 0.
    Denom = dot(Nx, U),

    [Div(dot(D, U), Denom), Div(dot(Nz, V), Denom), -Div(dot(Ny, V), Denom)].
