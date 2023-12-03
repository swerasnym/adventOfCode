-module(aoc2021_day22).

-export([run/2, profile/3, eprof/2]).

-record(cube, {state, x, y, z}).

run(Star, File) ->
    Data = read(File),
    case Star of
        star1 ->
            star1(Data);
        star1old ->
            star1old(Data);
        star2 ->
            star2(Data);
        _ ->
            Star1 = star1(Data),
            Star2 = star2(Data),
            {Star1, Star2}
    end.

profile(Star, File, Times) ->
    Data = read(File),
    case Star of
        star1 ->
            profile(fun() -> star1(Data) end, Times);
        star2 ->
            profile(fun() -> star2(Data) end, Times);
        _ ->
            Star1 = profile(fun() -> star1(Data) end, Times),
            Star2 = profile(fun() -> star2(Data) end, Times),
            {Star1, Star2}
    end.

profile(F, Times) ->
    Expected = F(),
    Results =
        [
            begin
                {Time, Expected} = timer:tc(F),
                Time
            end
         || _ <- lists:seq(1, Times)
        ],
    {Expected, lists:sum(Results) / Times / 1000}.

eprof(Star, File) ->
    eprof:start(),
    eprof:start_profiling([self()]),
    Result = run(Star, File),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop(),
    Result.

star1old(Data) ->
    Init = lists:filter(fun is_init/1, Data),
    Steps = lists:map(fun discretre/1, Init),
    Reactor =
        lists:foldl(fun({K, V}, Map) -> maps:put(K, V, Map) end, #{}, lists:flatten(Steps)),
    tools:count(on, Reactor).

star1(Data) ->
    Init = lists:filter(fun is_init/1, Data),
    Pices = remove_overlaps(Init),
    On = on(Pices),
    lists:sum(lists:map(fun volume/1, On)).

star2(Data) ->
    Pices = remove_overlaps(Data),
    On = on(Pices),
    lists:sum(lists:map(fun volume/1, On)).

read(File) ->
    [
        #cube{
            state = S,
            x = {Xmin, Xmax + 1},
            y = {Ymin, Ymax + 1},
            z = {Zmin, Zmax + 1}
        }
     || [S, Xmin, Xmax, Ymin, Ymax, Zmin, Zmax] <-
            tools:read_format(File, "~a x=~d..~d,y=~d..~d,z=~d..~d")
    ].

is_init(#cube{
    x = {Xmin, Xmax},
    y = {Ymin, Ymax},
    z = {Zmin, Zmax}
}) when
    Xmin >= -50, Xmax =< 50, Ymin >= -50, Ymax =< 50, Zmin >= -50, Zmax =< 50
->
    true;
is_init(#cube{}) ->
    false.

discretre(#cube{
    state = S,
    x = {Xmin, Xmax},
    y = {Ymin, Ymax},
    z = {Zmin, Zmax}
}) ->
    [
        {{X, Y, Z}, S}
     || X <- lists:seq(Xmin, Xmax - 1),
        Y <- lists:seq(Ymin, Ymax - 1),
        Z <- lists:seq(Zmin, Zmax - 1)
    ].

on(List) ->
    lists:filter(fun(#cube{state = S}) -> S == on end, List).

overlaps(#cube{x = {Min1, Max1}}, #cube{x = {Min2, Max2}}) when
    Min2 >= Max1; Min1 >= Max2
->
    false;
overlaps(#cube{y = {Min1, Max1}}, #cube{y = {Min2, Max2}}) when
    Min2 >= Max1; Min1 >= Max2
->
    false;
overlaps(#cube{z = {Min1, Max1}}, #cube{z = {Min2, Max2}}) when
    Min2 >= Max1; Min1 >= Max2
->
    false;
overlaps(_, _) ->
    true.

%% Rettunrs {[Overlapp], [NonOverlap]} parts of the second cube is always last.
split(Dir, C1, C2) ->
    L1 = {Min1, Max1} = element(Dir, C1),
    L2 = {Min2, Max2} = element(Dir, C2),
    case {L1, L2} of
        {L, L} ->
            {[C1, C2], []};
        _ when Min1 =< Min2, Min2 < Max1, Max1 =< Max2 ->
            {[setelement(Dir, C1, {Min2, Max1}), setelement(Dir, C2, {Min2, Max1})], [
                setelement(Dir, C1, {Min1, Min2}), setelement(Dir, C2, {Max1, Max2})
            ]};
        _ when Min1 =< Min2, Max1 >= Max2 ->
            {[setelement(Dir, C1, L2), C2], [
                setelement(Dir, C1, {Min1, Min2}), setelement(Dir, C1, {Max2, Max1})
            ]};
        _ when Min2 =< Min1, Min1 < Max2, Max2 =< Max1 ->
            {[setelement(Dir, C1, {Min1, Max2}), setelement(Dir, C2, {Min1, Max2})], [
                setelement(Dir, C1, {Max2, Max1}), setelement(Dir, C2, {Min2, Min1})
            ]};
        _ when Min2 =< Min1, Max2 >= Max1 ->
            {[C1, setelement(Dir, C2, L1)], [
                setelement(Dir, C2, {Min2, Min1}), setelement(Dir, C2, {Max1, Max2})
            ]}
    end.

split(C1, C2) ->
    {[X1, X2], Xn} = split(#cube.x, C1, C2),
    {[Y1, Y2], Yn} = split(#cube.y, X1, X2),
    {[_Z1, Z2], Zn} = split(#cube.z, Y1, Y2),

    % Z1 discarded since that contains the overlappng part in C1 that we are replaceing.
    lists:filter(fun(V) -> volume(V) /= 0 end, Xn ++ Yn ++ Zn ++ [Z2]).

volume(#cube{
    x = {Xmin, Xmax},
    y = {Ymin, Ymax},
    z = {Zmin, Zmax}
}) ->
    (Xmax - Xmin) * (Ymax - Ymin) * (Zmax - Zmin).

remove_overlaps([C1 | Rest]) ->
    remove_overlaps([C1], Rest).

remove_overlaps(Done, []) ->
    Done;
remove_overlaps(Done, [C2 | Rest]) ->
    case lists:partition(fun(C) -> overlaps(C, C2) end, Done) of
        {[Hd | Overlaps], NoOverlaps} ->
            Pices = split(Hd, C2),
            remove_overlaps(NoOverlaps ++ on(remove_overlaps(Overlaps, Pices)), Rest);
        {[], Done} ->
            remove_overlaps([C2 | Done], Rest)
    end.
