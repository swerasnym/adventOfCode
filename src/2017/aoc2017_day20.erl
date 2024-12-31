-module(aoc2017_day20).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day20_ex.txt", star1, 0},
        {"examples/2017/day20_ex2.txt", star2, 1}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 20},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Raw) ->
    Normal = [normalize(R) || R <- Raw],
    Numbered = lists:enumerate(0, Normal),
    Sorted = lists:sort(fun order/2, Numbered),
    {N, _} = hd(Sorted),
    N.

star2(Raw) ->
    Normal = [normalize(R) || R <- Raw],
    Numbered = lists:enumerate(0, Normal),
    Sorted = lists:sort(fun order/2, Numbered),
    Time = time(Sorted, 1),
    io:format("Simulating ~p time steps ~n", [Time]),

    length(tools:repeat(Time, fun step/1, lists:usort(Raw))).

read(File) ->
    tools:group(3, tools:group(3, tools:read_integers(File, "\n =<>pva,"), list)).

sign({P, 0, 0}) -> tools:sign(P);
sign({_, V, 0}) -> tools:sign(V);
sign({_, _, A}) -> tools:sign(A).

normalize({P, V, A}) ->
    D = [sign(N) || N <- lists:zip3(P, V, A)],
    {aoc_vector:mul(P, D), aoc_vector:mul(V, D), aoc_vector:mul(A, D)}.

sum(P) -> lists:sum(P).
dist(P) -> lists:sum([abs(N) || N <- P]).

order({N1, {P1, V1, A1}}, {N2, {P2, V2, A2}}) ->
    case {sum(P1) == sum(P2), sum(V1) == sum(V2), sum(A1) == sum(A2)} of
        {_, _, false} -> sum(A1) < sum(A2);
        {_, false, true} -> sum(V1) < sum(V2);
        {false, true, true} -> sum(P1) < sum(P2);
        {true, true, true} -> N1 < N2
    end.

time([_], Max) ->
    Max;
time([{_, S1} | Rest], T) ->
    S1T = at(T, S1),
    T1 = lists:max([find_t(T, S1T, at(T, S2)) || {_, S2} <- Rest]),
    time(Rest, max(T, T1)).

find_t(T, S1T, S2T) ->
    P1 = element(1, S1T),
    P2 = element(1, S2T),
    [X, Y, Z] = P2,
    case X >= 0 andalso Y >= 0 andalso Z >= 0 andalso dist(P2) >= dist(P1) of
        true ->
            T;
        false ->
            find_t(T + 1, update(S1T), update(S2T))
    end.

step(List) ->
    Updated = lists:sort([update(L) || L <- List]),
    collide(Updated, false, []).

at(T, {P, V, A}) ->
    VT = aoc_vector:add(V, aoc_vector:mul(T, A)),
    PT = aoc_vector:sum([P, aoc_vector:mul(T, V), aoc_vector:mul((T * (T + 1)) div 2, A)]),
    {PT, VT, A}.

update({P, V, A}) ->
    VT = aoc_vector:add(V, A),
    PT = aoc_vector:add(P, VT),
    {PT, VT, A}.

collide([], _, Out) ->
    Out;
collide([{P, _, _} | [{P, _, _} | _] = Rest], _, Out) ->
    collide(Rest, true, Out);
collide([_ | Rest], true, Out) ->
    collide(Rest, false, Out);
collide([S | Rest], false, Out) ->
    collide(Rest, false, [S | Out]).
