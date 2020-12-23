-module(day23).

-export([run/2]).

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

star1(Cups) ->
    move(100, Cups).

star2(Cups) ->
    Min = lists:max(Cups) + 1,
    Max = 1000000,

    Map = make_circular(Cups ++ lists:seq(Min, Max), lists:last(Cups)),
    move_map(10000000, hd(Cups), Max, Map).

make_circular([First, Next | Rest], Last) ->
    %% previous, next
    make_circular(First, [Next | Rest], #{First => {Last, Next}}, First).

make_circular(Previous, [Last], Map, First) ->
    Map#{Last => {Previous, First}};
make_circular(Previous, [Current, Next | Rest], Map, First) ->
    make_circular(Current, [Next | Rest], Map#{Current => {Previous, Next}}, First).

prev(Current, Map) ->
    {Prev, _Next} = maps:get(Current, Map),
    Prev.

next(Current, Map) ->
    {_Prev, Next} = maps:get(Current, Map),
    Next.

move_map(0, _Current, _Max, Map) ->
    C1 = next(1, Map),
    C2 = next(C1, Map),
    C1 * C2;
move_map(Times, Current, Max, Map) ->
    P1 = next(Current, Map),
    P2 = next(P1, Map),
    P3 = next(P2, Map),
    Pickup = [P1, P2, P3],

    P4 = next(P3, Map),

    Dest = destination(Current - 1, Pickup, Max),

    move_map(Times - 1,
             P4,
             Max,
             if Dest /= P4 ->
                    Map#{Dest => {prev(Dest, Map), P1},
                         P1 => {Dest, P2},
                         P3 => {P2, next(Dest, Map)},
                         Current => {prev(Current, Map), P4},
                         P4 => {Current, next(P4, Map)}};
                Dest == P4 ->
                    Map#{Dest => {prev(Dest, Map), P1},
                         P1 => {Dest, P2},
                         P3 => {P2, next(Dest, Map)},
                         Current => {prev(Current, Map), P4}}
             end).

read(example) ->
    [3, 8, 9, 1, 2, 5, 4, 6, 7];
read(_) ->
    [3, 6, 4, 2, 8, 9, 7, 1, 5].

move(Times, Cups) ->
    move(Times, Cups, lists:max(Cups)).

move(0, Cups, _Max) ->
    Cups;
move(Times, [Current, P1, P2, P3 | Rest], Max) ->
    Pickup = [P1, P2, P3],
    Destination = destination(Current - 1, Pickup, Max),
    move(Times - 1, insert(Pickup, Destination, Rest, []) ++ [Current], Max).

destination(0, Pickup, Max) ->
    destination(Max, Pickup, Max);
destination(Dest, [P1, P2, P3], _Max) when Dest /= P1, Dest /= P2, Dest /= P3 ->
    Dest;
destination(Dest, Pickup, Max) ->
    destination(Dest - 1, Pickup, Max).

insert(Pickup, Destination, [Destination | Rest], Previous) ->
    Previous ++ [Destination] ++ Pickup ++ Rest;
insert(Pickup, Destination, [First | Rest], Previous) ->
    insert(Pickup, Destination, Rest, Previous ++ [First]).
