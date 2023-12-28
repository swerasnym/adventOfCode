-module(aoc2021_day22).
-behaviour(aoc_solution).

-record(cube, {state, x, y, z}).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 22},
        examples => Examples,
        all => [star1, {star1, old}, star2]
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data, old) ->
    Init = lists:filter(fun is_init/1, Data),
    Steps = lists:map(fun discrete/1, Init),
    Reactor =
        lists:foldl(fun({K, V}, Map) -> maps:put(K, V, Map) end, #{}, lists:flatten(Steps)),
    tools:count(on, Reactor).

star1(Data) ->
    Init = lists:filter(fun is_init/1, Data),
    Pisces = remove_overlaps(Init),
    On = on(Pisces),
    lists:sum(lists:map(fun volume/1, On)).

star2(Data) ->
    Pisces = remove_overlaps(Data),
    On = on(Pisces),
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
            tools:read_multiple_formats(File, "~a x=~d..~d,y=~d..~d,z=~d..~d")
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

discrete(#cube{
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

%% Returns {[Overlap], [NonOverlap]} parts of the second cube is always last.
split(Dir, C1, C2) ->
    L1 = {Min1, Max1} = element(Dir, C1),
    L2 = {Min2, Max2} = element(Dir, C2),
    case {L1, L2} of
        {L, L} ->
            {[C1, C2], []};
        _ when Min1 =< Min2, Min2 < Max1, Max1 =< Max2 ->
            {
                [
                    erlang:setelement(Dir, C1, {Min2, Max1}),
                    erlang:setelement(Dir, C2, {Min2, Max1})
                ],
                [
                    erlang:setelement(Dir, C1, {Min1, Min2}),
                    erlang:setelement(Dir, C2, {Max1, Max2})
                ]
            };
        _ when Min1 =< Min2, Max1 >= Max2 ->
            {[erlang:setelement(Dir, C1, L2), C2], [
                erlang:setelement(Dir, C1, {Min1, Min2}),
                erlang:setelement(Dir, C1, {Max2, Max1})
            ]};
        _ when Min2 =< Min1, Min1 < Max2, Max2 =< Max1 ->
            {
                [
                    erlang:setelement(Dir, C1, {Min1, Max2}),
                    erlang:setelement(Dir, C2, {Min1, Max2})
                ],
                [
                    erlang:setelement(Dir, C1, {Max2, Max1}),
                    erlang:setelement(Dir, C2, {Min2, Min1})
                ]
            };
        _ when Min2 =< Min1, Max2 >= Max1 ->
            {[C1, erlang:setelement(Dir, C2, L1)], [
                erlang:setelement(Dir, C2, {Min2, Min1}),
                erlang:setelement(Dir, C2, {Max1, Max2})
            ]}
    end.

split(C1, C2) ->
    {[X1, X2], Xn} = split(#cube.x, C1, C2),
    {[Y1, Y2], Yn} = split(#cube.y, X1, X2),
    {[_Z1, Z2], Zn} = split(#cube.z, Y1, Y2),

    % Z1 discarded since that contains the overlapping part in C1 that we are replacing.
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
            Pisces = split(Hd, C2),
            remove_overlaps(NoOverlaps ++ on(remove_overlaps(Overlaps, Pisces)), Rest);
        {[], Done} ->
            remove_overlaps([C2 | Done], Rest)
    end.
