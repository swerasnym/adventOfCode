-module(aoc2018_day23).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2018/day23_ex.txt", star1, 7},
        {"examples/2018/day23_ex2.txt", star2, 36}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 23},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Bots) ->
    {RStrong, PosStrong} = lists:max(Bots),
    length([Pos || {_, Pos} <- Bots, aoc_vector:manhattan(PosStrong, Pos) =< RStrong]).

star2(Bots) ->
    EnumeratedBots = lists:enumerate(Bots),
    StartsAndStops = [starts_ans_stops(Base, EnumeratedBots) || Base <- basis_vectors()],
    Joined = [join(S, [], []) || S <- StartsAndStops],
    Best = find_best_clusters(Joined, {lists:seq(1, length(Bots)), []}, [{0, []}]),
    [{_, Clusters}] = Best,
    Intersections = [erlang:list_to_tuple(I) || I <- intersections(Clusters)],
    io:format("~p~n", [Intersections]),

    {XL, YL, ZL} = lists:unzip3(Intersections),

    Points = [
        {X, Y, Z}
     || X <- lists:seq(lists:min(XL), lists:max(XL)),
        Y <- lists:seq(lists:min(YL), lists:max(YL)),
        Z <- lists:seq(lists:min(ZL), lists:max(ZL))
    ],

    DistPoints = lists:sort([{aoc_vector:manhattan(I, {0, 0, 0}), I} || I <- Points]),

    {_, Dist} = hd(lists:sort(find_matching(Bots, DistPoints))),
    Dist.

read(File) ->
    Bots = tools:read_multiple_formats(File, "pos=<~d,~d,~d>, r=~d"),
    [{R, {X, Y, Z}} || [X, Y, Z, R] <- Bots].

find_matching(Bots, Points) ->
    [
        {
            -length([
                R
             || {R, Pos} <- Bots, aoc_vector:manhattan(Point, Pos) =< R
            ]),
            Dist
        }
     || {Dist, Point} <- Points
    ].

find_best_clusters([[] | _], _, Best) ->
    Best;
find_best_clusters(
    [Level | OtherPlanes], {Bots, Planes}, [{BestNumber, _} | _] = CurrentBest
) ->
    [{N, HeadPlanes, PlaneBots} | Rest] = Level,
    case N < BestNumber of
        true ->
            CurrentBest;
        false ->
            RemainingBots = tools:overlap(Bots, PlaneBots),
            case length(RemainingBots) < BestNumber of
                true ->
                    find_best_clusters([Rest | OtherPlanes], {Bots, Planes}, CurrentBest);
                false ->
                    Best = find_best_clusters(
                        OtherPlanes, {RemainingBots, [HeadPlanes | Planes]}, CurrentBest
                    ),
                    find_best_clusters([Rest | OtherPlanes], {Bots, Planes}, Best)
            end
    end;
find_best_clusters([], {Bots, Planes}, [{BestNumber, _} | _] = CurrentBest) ->
    case length(Bots) of
        N when N < BestNumber ->
            CurrentBest;
        N when N > BestNumber ->
            io:format("~p~n", [{N, Planes}]),
            [{N, Planes}];
        BestNumber ->
            [{BestNumber, Planes} | CurrentBest]
    end.

starts_ans_stops(Base, EnumeratedBots) ->
    tools:reverse_sort(lists:flatten([start_stop(Base, Bot) || Bot <- EnumeratedBots])).

basis_vectors() ->
    [{1, 1, 1}, {1, -1, 1}, {-1, 1, 1}, {-1, -1, 1}].

join([{_, stop, N}], [N2], Acc) when N == N2 ->
    tools:reverse_sort(Acc);
join([A, B | Rest], Active, Acc) ->
    {Range, NewActive} = join(A, B, Active),
    join([B | Rest], NewActive, [Range | Acc]);
join({{D, Base}, Type, N}, {{D2, Base}, _, _}, Active) ->
    case Type of
        start ->
            NewActive = [N | Active],
            Range = {length(NewActive), {{D, Base}, {D2, Base}}, lists:sort(NewActive)};
        stop ->
            NewActive = Active -- [N],
            Range = {length(NewActive), {{D, Base}, {D2, Base}}, lists:sort(NewActive)}
    end,

    {Range, NewActive}.

start_stop(Base, {I, Bot}) ->
    [{plane_top(Base, Bot), start, I}, {plane_bot(Base, Bot), stop, I}].

plane_top(Base, {R, Pos}) ->
    {aoc_vector:dot(Base, Pos) + R + 1, Base}.
plane_bot(Base, {R, Pos}) ->
    {aoc_vector:dot(Base, Pos) - R - 1, Base}.

intersections([{At, Ab}, {Bt, Bb}, {Ct, Cb}, {Dt, Db}]) ->
    lists:usort([
        aoc_vector:three_plane_intersection_i(At, Bt, Ct),
        aoc_vector:three_plane_intersection_i(At, Bt, Cb),
        aoc_vector:three_plane_intersection_i(At, Bb, Ct),
        aoc_vector:three_plane_intersection_i(At, Bb, Cb),
        aoc_vector:three_plane_intersection_i(Ab, Bt, Ct),
        aoc_vector:three_plane_intersection_i(Ab, Bt, Cb),
        aoc_vector:three_plane_intersection_i(Ab, Bb, Ct),
        aoc_vector:three_plane_intersection_i(Ab, Bb, Cb),

        aoc_vector:three_plane_intersection_i(At, Bt, Dt),
        aoc_vector:three_plane_intersection_i(At, Bt, Db),
        aoc_vector:three_plane_intersection_i(At, Bb, Dt),
        aoc_vector:three_plane_intersection_i(At, Bb, Db),
        aoc_vector:three_plane_intersection_i(Ab, Bt, Dt),
        aoc_vector:three_plane_intersection_i(Ab, Bt, Db),
        aoc_vector:three_plane_intersection_i(Ab, Bb, Dt),
        aoc_vector:three_plane_intersection_i(Ab, Bb, Db),

        aoc_vector:three_plane_intersection_i(At, Ct, Dt),
        aoc_vector:three_plane_intersection_i(At, Ct, Db),
        aoc_vector:three_plane_intersection_i(At, Cb, Dt),
        aoc_vector:three_plane_intersection_i(At, Cb, Db),
        aoc_vector:three_plane_intersection_i(Ab, Ct, Dt),
        aoc_vector:three_plane_intersection_i(Ab, Ct, Db),
        aoc_vector:three_plane_intersection_i(Ab, Cb, Dt),
        aoc_vector:three_plane_intersection_i(Ab, Cb, Db),

        aoc_vector:three_plane_intersection_i(Bt, Ct, Dt),
        aoc_vector:three_plane_intersection_i(Bt, Ct, Db),
        aoc_vector:three_plane_intersection_i(Bt, Cb, Dt),
        aoc_vector:three_plane_intersection_i(Bt, Cb, Db),
        aoc_vector:three_plane_intersection_i(Bb, Ct, Dt),
        aoc_vector:three_plane_intersection_i(Bb, Ct, Db),
        aoc_vector:three_plane_intersection_i(Bb, Cb, Dt),
        aoc_vector:three_plane_intersection_i(Bb, Cb, Db)
    ]).
