-module(aoc2025_day9).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2025/day9_ex.txt", star1, 50},
        {"examples/2025/day9_ex.txt", star2, 24}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2025, 9},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Points) ->
    lists:max([area(P1, P2) || P1 <- Points, P2 <- Points, P1 < P2]).

star2(Points) ->
    Segments = border_segments(Points ++ [hd(Points)], []),
    Rectangles = tools:reverse_sort([
        {area(P1, P2), {P1, P2}}
     || P1 <- Points, P2 <- Points, P1 < P2
    ]),
    {value, {Area, _}} = lists:search(fun({_, Rect}) -> inside(Rect, Segments) end, Rectangles),
    Area.

read(File) ->
    tools:group(2, tools:read_integers(File, ",\n")).

area({X1, Y1}, {X2, Y2}) ->
    (abs(X1 - X2) + 1) * (abs(Y1 - Y2) + 1).

inside({{X1, Y1}, {X2, Y2}}, Border) ->
    I1 = {{X1, Y1}, {X1, Y2}},
    I2 = {{X2, Y1}, {X2, Y2}},
    I3 = {{X1, Y1}, {X2, Y1}},
    I4 = {{X1, Y2}, {X2, Y2}},

    non_inter(I1, Border) andalso non_inter(I2, Border) andalso non_inter(I3, Border) andalso
        non_inter(I4, Border).

border_segments([_], Border) ->
    Border;
border_segments([P1, P2 | Rest], Border) ->
    border_segments([P2 | Rest], [{P1, P2} | Border]).

intersect({{X1, Y1}, {X2, Y2}}, P) ->
    [Xmin, Xmax] = lists:sort([X1, X2]),
    [Ymin, Ymax] = lists:sort([Y1, Y2]),
    case P of
        {{X3, Y}, {X4, Y}} ->
            Min = min(X3, X4),
            Max = max(X3, X4),
            (Ymin < Y andalso Y < Ymax) andalso
                ((Min =< Xmin andalso Xmin < Max) orelse (Min < Xmax andalso Xmax =< Max));
        {{X, Y3}, {X, Y4}} ->
            Min = min(Y3, Y4),
            Max = max(Y3, Y4),
            (Xmin < X andalso X < Xmax) andalso
                ((Min =< Ymin andalso Ymin < Max) orelse (Min < Ymax andalso Ymax =< Max))
    end.

non_inter(_, []) ->
    true;
non_inter(Side, [Side | Rest]) ->
    non_inter(Side, Rest);
non_inter(Side, [I | Rest]) ->
    case intersect(Side, I) of
        true -> false;
        false -> non_inter(Side, Rest)
    end.
