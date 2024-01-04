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
    Joined = [join(S, [], [], []) || S <- StartsAndStops],
    Best = find_best_clusters(Joined, {lists:seq(1, length(Bots)), []}, [{0, []}]),
    Intersections = lists:usort([
        to_integer_tuple(I)
     || {_, Clusters} <- Best, I <- intersections(Clusters), all_integers(I)
    ]),
    io:format("~p~n", [Intersections]),

    DistPoints = lists:sort([{aoc_vector:manhattan(I, {0, 0, 0}), I} || I <- Intersections]),
    Matching = lists:sort(find_matching(Bots, DistPoints)),
    io:format("~p~n", [Matching]),
    {_, Dist, _} = hd(Matching),
    Dist.

read(File) ->
    Bots = tools:read_multiple_formats(File, "pos=<~d,~d,~d>, r=~d"),
    [{R, {X, Y, Z}} || [X, Y, Z, R] <- Bots].

is_int(N) when is_integer(N) ->
    true;
is_int(N) when is_float(N) ->
    N == erlang:trunc(N).

all_integers(L) ->
    lists:all(fun is_int/1, L).

to_integer_tuple(L) ->
    erlang:list_to_tuple([erlang:trunc(N) || N <- L]).

find_matching(Bots, Points) ->
    [
        {
            -length([
                R
             || {R, Pos} <- Bots, aoc_vector:manhattan(Point, Pos) =< R
            ]),
            Dist,
            Point
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
                false when length(RemainingBots) == 0 ->
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
            io:format("New region ~p~n", [{N, Planes}]),
            [{N, Planes}];
        BestNumber ->
            io:format("Additional regions ~p~n", [{BestNumber, Planes}]),
            [{BestNumber, Planes} | CurrentBest]
    end.

starts_ans_stops(Base, EnumeratedBots) ->
    tools:reverse_sort(lists:flatten([start_stop(Base, Bot) || Bot <- EnumeratedBots])).

basis_vectors() ->
    [{1, 1, 1}, {1, -1, 1}, {-1, 1, 1}, {-1, -1, 1}].

join([{_, delete, N}], [N], _, Acc) ->
    tools:reverse_sort(Acc);
join([{{D, Base}, Type, N}, {{D, Base}, _, _} = B | Rest], Active, Remove, Acc) ->
    case Type of
        insert ->
            join([B | Rest], lists:merge([N], Active), Remove, Acc);
        delete ->
            join([B | Rest], Active, [N | Remove], Acc)
    end;
join([A, B | Rest], Active, Remove, Acc) ->
    {Range, NewActive} = join(A, B, Active),
    join([B | Rest], NewActive -- Remove, [], [Range | Acc]).

join({{D, Base}, Type, N} = A, {{D2, Base}, _, _} = B, Active) ->
    case D == D2 of
        true ->
            io:format("~p ~p~n", [A, B]);
        _ ->
            ok
    end,

    case Type of
        insert ->
            NewActive = lists:merge([N], Active),
            Range = {length(NewActive), {{D, Base}, {D2, Base}}, NewActive};
        delete ->
            NewActive = Active -- [N],
            Range = {length(Active), {{D, Base}, {D2, Base}}, Active}
    end,

    {Range, NewActive}.

start_stop(Base, {I, Bot}) ->
    [{plane_top(Base, Bot), insert, I}, {plane_bot(Base, Bot), delete, I}].

plane_top(Base, {R, Pos}) ->
    {aoc_vector:dot(Base, Pos) + R, Base}.
plane_bot(Base, {R, Pos}) ->
    {aoc_vector:dot(Base, Pos) - R, Base}.

intersections([{At, Ab}, {Bt, Bb}, {Ct, Cb}, {Dt, Db}]) ->
    [
        aoc_vector:three_plane_intersection(At, Bt, Ct),
        aoc_vector:three_plane_intersection(At, Bt, Cb),
        aoc_vector:three_plane_intersection(At, Bb, Ct),
        aoc_vector:three_plane_intersection(At, Bb, Cb),
        aoc_vector:three_plane_intersection(Ab, Bt, Ct),
        aoc_vector:three_plane_intersection(Ab, Bt, Cb),
        aoc_vector:three_plane_intersection(Ab, Bb, Ct),
        aoc_vector:three_plane_intersection(Ab, Bb, Cb),

        aoc_vector:three_plane_intersection(At, Bt, Dt),
        aoc_vector:three_plane_intersection(At, Bt, Db),
        aoc_vector:three_plane_intersection(At, Bb, Dt),
        aoc_vector:three_plane_intersection(At, Bb, Db),
        aoc_vector:three_plane_intersection(Ab, Bt, Dt),
        aoc_vector:three_plane_intersection(Ab, Bt, Db),
        aoc_vector:three_plane_intersection(Ab, Bb, Dt),
        aoc_vector:three_plane_intersection(Ab, Bb, Db),

        aoc_vector:three_plane_intersection(At, Ct, Dt),
        aoc_vector:three_plane_intersection(At, Ct, Db),
        aoc_vector:three_plane_intersection(At, Cb, Dt),
        aoc_vector:three_plane_intersection(At, Cb, Db),
        aoc_vector:three_plane_intersection(Ab, Ct, Dt),
        aoc_vector:three_plane_intersection(Ab, Ct, Db),
        aoc_vector:three_plane_intersection(Ab, Cb, Dt),
        aoc_vector:three_plane_intersection(Ab, Cb, Db),

        aoc_vector:three_plane_intersection(Bt, Ct, Dt),
        aoc_vector:three_plane_intersection(Bt, Ct, Db),
        aoc_vector:three_plane_intersection(Bt, Cb, Dt),
        aoc_vector:three_plane_intersection(Bt, Cb, Db),
        aoc_vector:three_plane_intersection(Bb, Ct, Dt),
        aoc_vector:three_plane_intersection(Bb, Ct, Db),
        aoc_vector:three_plane_intersection(Bb, Cb, Dt),
        aoc_vector:three_plane_intersection(Bb, Cb, Db)
    ].
