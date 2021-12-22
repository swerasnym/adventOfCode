-module(aoc2021_day22).

-export([run/2, profile/3, eprof/2, split/2]).

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
        [begin
             {Time, Expected} = timer:tc(F),
             Time
         end
         || _ <- lists:seq(1, Times)],
    {Expected, lists:sum(Results) / Times / 1000}.

eprof(Star, File) ->
    eprof:start(),
    eprof:start_profiling([self()]),
    Result = run(Star, File),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop(),
    Result.

star1(Data) ->
    Init = lists:filter(fun is_init/1, Data),
    Steps = lists:map(fun discretre/1, Init),
    Reactor =
        lists:foldl(fun({K, V}, Map) -> maps:put(K, V, Map) end, #{}, lists:flatten(Steps)),
    tools:count(on, Reactor).

star2(Data) ->
    %% Init = lists:filter(fun is_init/1, Data),
    Pices = remove_overlaps(Data),
    On = lists:filter(fun({S, _, _, _}) -> S == on end, Pices),
    lists:sum(
        lists:map(fun volume/1, On)).

    %% Steps = lists:map(fun discretre/1, Init),
    %% Reactor = lists:foldl(fun({K,V}, Map) -> maps:put(K, V, Map) end, #{}, lists:flatten(Steps)),
    %% tools:count(on, Reactor).

read(File) ->
    [{S, {X_min, X_Max + 1}, {Y_min, Y_Max + 1}, {Z_min, Z_Max + 1}}
     || [S, X_min, X_Max, Y_min, Y_Max, Z_min, Z_Max]
            <- tools:read_format(File, "~a x=~d..~d,y=~d..~d,z=~d..~d")].

is_init({_, {X_min, X_Max}, {Y_min, Y_Max}, {Z_min, Z_Max}})
    when X_min >= -50, X_Max =< 50, Y_min >= -50, Y_Max =< 50, Z_min >= -50, Z_Max =< 50 ->
    true;
is_init(_) ->
    false.

discretre({S, {X_min, X_Max}, {Y_min, Y_Max}, {Z_min, Z_Max}}) ->
    [{{X, Y, Z}, S}
     || X <- lists:seq(X_min, X_Max - 1),
        Y <- lists:seq(Y_min, Y_Max - 1),
        Z <- lists:seq(Z_min, Z_Max - 1)].

overlaps({_S1, {_X_min1, X_Max1}, _Y1, _Z1}, {_S2, {X_min2, _X_Max2}, _Y2, _Z2})
    when X_min2 >= X_Max1 ->
    false;
overlaps({_S1, {X_min1, _X_Max1}, _Y1, _Z1}, {_S2, {_X_min2, X_Max2}, _Y2, _Z2})
    when X_min1 >= X_Max2 ->
    false;
overlaps({_S1, _X1, {_Y_min1, Y_Max1}, _Z1}, {_S2, _X2, {Y_min2, _Y_Max2}, _Z2})
    when Y_min2 >= Y_Max1 ->
    false;
overlaps({_S1, _X1, {Y_min1, _Y_Max1}, _Z1}, {_S2, _X2, {_Y_min2, Y_Max2}, _Z2})
    when Y_min1 >= Y_Max2 ->
    false;
overlaps({_S1, _X1, _Y1, {_Z_min1, Z_Max1}}, {_S2, _X2, _Y2, {Z_min2, _Z_Max2}})
    when Z_min2 >= Z_Max1 ->
    false;
overlaps({_S1, _X1, _Y1, {Z_min1, _Z_Max1}}, {_S2, _X2, _Y2, {_Z_min2, Z_Max2}})
    when Z_min1 >= Z_Max2 ->
    false;
overlaps(_, _) ->
    true.

split_x(N, {S1, {X1_min, X1_Max}, Y1, Z1}, {S2, {X2_min, X2_Max}, Y2, Z2})
    when N < 2, X1_min =< X2_min, X2_min < X1_Max, X1_Max =< X2_Max ->
    % {[Overlapp], [NonOverlap]}
    {[{S1, {X2_min, X1_Max}, Y1, Z1}, {S2, {X2_min, X1_Max}, Y2, Z2}],
     [{S1, {X1_min, X2_min}, Y1, Z1}, {S2, {X1_Max, X2_Max}, Y2, Z2}]};
split_x(N, {S1, {X1_min, X1_Max}, Y1, Z1}, {S2, {X2_min, X2_Max}, Y2, Z2})
    when N < 2, X1_min =< X2_min, X1_Max >= X2_Max ->
    {[{S1, {X2_min, X2_Max}, Y1, Z1}, {S2, {X2_min, X2_Max}, Y2, Z2}],
     [{S1, {X1_min, X2_min}, Y1, Z1}, {S1, {X2_Max, X1_Max}, Y1, Z1}]};
split_x(N, C1, C2) when N < 2 ->
    {[O2, O1], No} = split_x(N + 1, C2, C1),
    {[O1, O2], No};
split_x(2, {_, X, _, _} = C1, {_, X, _, _} = C2) ->
    {[C1, C2], []}.

split_y(N, {S1, X1, {Y1_min, Y1_Max}, Z1}, {S2, X2, {Y2_min, Y2_Max}, Z2})
    when N < 2, Y1_min =< Y2_min, Y2_min < Y1_Max, Y1_Max =< Y2_Max ->
    % {[Overlapp], [NonOverlap]}
    {[{S1, X1, {Y2_min, Y1_Max}, Z1}, {S2, X2, {Y2_min, Y1_Max}, Z2}],
     [{S1, X1, {Y1_min, Y2_min}, Z1}, {S2, X2, {Y1_Max, Y2_Max}, Z2}]};
split_y(N, {S1, X1, {Y1_min, Y1_Max}, Z1}, {S2, X2, {Y2_min, Y2_Max}, Z2})
    when N < 2, Y1_min =< Y2_min, Y1_Max >= Y2_Max ->
    {[{S1, X1, {Y2_min, Y2_Max}, Z1}, {S2, X2, {Y2_min, Y2_Max}, Z2}],
     [{S1, X1, {Y1_min, Y2_min}, Z1}, {S1, X1, {Y2_Max, Y1_Max}, Z1}]};
split_y(N, C1, C2) when N < 2 ->
    {[O2, O1], No} = split_y(N + 1, C2, C1),
    {[O1, O2], No};
split_y(2, {_, _, Y, _} = C1, {_, _, Y, _} = C2) ->
    {[C1, C2], []}.

split_z(N, {S1, X1, Y1, {Z1_min, Z1_Max}}, {S2, X2, Y2, {Z2_min, Z2_Max}})
    when N < 2, Z1_min =< Z2_min, Z2_min < Z1_Max, Z1_Max =< Z2_Max ->
    % {[Overlapp], [NonOverlap]}
    {[{S1, X1, Y1, {Z2_min, Z1_Max}}, {S2, X2, Y2, {Z2_min, Z1_Max}}],
     [{S1, X1, Y1, {Z1_min, Z2_min}}, {S2, X2, Y2, {Z1_Max, Z2_Max}}]};
split_z(N, {S1, X1, Y1, {Z1_min, Z1_Max}}, {S2, X2, Y2, {Z2_min, Z2_Max}})
    when N < 2, Z1_min =< Z2_min, Z1_Max >= Z2_Max ->
    {[{S1, X1, Y1, {Z2_min, Z2_Max}}, {S2, X2, Y2, {Z2_min, Z2_Max}}],
     [{S1, X1, Y1, {Z1_min, Z2_min}}, {S1, X1, Y1, {Z2_Max, Z1_Max}}]};
split_z(N, C1, C2) when N < 2 ->
    {[O2, O1], No} = split_z(N + 1, C2, C1),
    {[O1, O2], No};
split_z(2, {_, _, _, Z} = C1, {_, _, _, Z} = C2) ->
    {[C1, C2], []}.

split(C1, C2) ->
    {[X1, X2], Xn} = split_x(0, C1, C2),
    {[Y1, Y2], Yn} = split_y(0, X1, X2),
    {[{S1, X, Y, Z} = Z1, {S2, X, Y, Z} = Z2], Zn} = split_z(0, Y1, Y2),
    S1 = hd(tuple_to_list(C1)),
    S2 = hd(tuple_to_list(C2)),

    lists:filter(fun(V) -> volume(V) /= 0 end, Xn ++ Yn ++ Zn ++ [Z2]).

volume({_, {Xmin, Xmax}, {Ymin, Ymax}, {Zmin, Zmax}}) ->
    (Xmax - Xmin) * (Ymax - Ymin) * (Zmax - Zmin).

remove_overlaps([C1 | Rest]) ->
    remove_overlaps([C1], Rest).

remove_overlaps(NonOverlapping, []) ->
    NonOverlapping;
remove_overlaps(NonOverlapping, [C2 | Rest]) ->
    remove_overlaps(non_overlapping(NonOverlapping, C2, []), Rest).

non_overlapping([], C2, Done) ->
    lists:reverse(Done, [C2]);
non_overlapping([C1 | NonOverlapping], C2, Done) ->
    case overlaps(C1, C2) of
        false ->
            non_overlapping(NonOverlapping, C2, [C1 | Done]);
        true ->
            Pices = split(C1, C2),
            F = fun(C, Acc) -> non_overlapping(Acc, C, []) end,
            Done ++ lists:foldl(F, NonOverlapping, Pices)
    end.
