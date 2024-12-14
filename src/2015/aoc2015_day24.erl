-module(aoc2015_day24).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2015/day24_ex.txt", star1, 99},
        {"examples/2015/day24_ex.txt", star2, 44}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 24},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Packets) ->
    Sum = lists:sum(Packets),
    PerGroup = Sum div 3,

    G1 = fill(PerGroup, Packets),
    Rated = [{rate(P), Other} || {P, Other} <- G1],
    find_best(PerGroup, lists:sort(Rated)).

star2(Packets) ->
    Sum = lists:sum(Packets),
    PerGroup = Sum div 4,
    G1 = fill(PerGroup, Packets),
    Rated = [{rate(P), Other} || {P, Other} <- G1],
    find_best2(PerGroup, lists:sort(Rated)).

read(File) ->
    tools:reverse_sort(tools:read_integers(File)).

fill(PerGroup, Packets) ->
    fill(PerGroup, Packets, [], [], []).

fill(0, Packets, This, Other, Acc) ->
    [{This, Packets ++ Other} | Acc];
fill(N, _, _, _, Acc) when N < 0 ->
    Acc;
fill(_, [], _, _, Acc) ->
    Acc;
fill(N, [Packet | Rest], This, Other, Acc) ->
    Acc1 = fill(N - Packet, Rest, [Packet | This], Other, Acc),
    fill(N, Rest, This, [Packet | Other], Acc1).

rate(Packets) ->
    {length(Packets), tools:product(Packets)}.

find_best(PerGroup, [{{_, QE}, Packets} | Rest]) ->
    case fill(PerGroup, Packets) of
        [] ->
            find_best(PerGroup, Rest);
        _ ->
            QE
    end.

find_best2(PerGroup, [{Rank, Packets} | Rest]) ->
    case fill(PerGroup, Packets) of
        [] ->
            find_best2(PerGroup, Rest);
        G2 ->
            Rated = [{Rank, Other} || {_, Other} <- G2],
            find_best(PerGroup, Rated)
    end.
