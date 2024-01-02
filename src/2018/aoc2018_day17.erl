-module(aoc2018_day17).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2018/day17_ex.txt", star1, 57},
        {"examples/2018/day17_ex2.txt", star1, 122},
        {"examples/2018/day17_ex3.txt", star1, 236},
        {"examples/2018/day17_ex.txt", star2, 29}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 17},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    {_, {Ymin, _}} = Limits = tools:min_max_grid(Map),
    Filled = fill([{500, 1}], [], Limits, Map#{{500, 1} => $|, {500, 0} => $+}),
    tools:print_grid(Filled),
    Count = tools:count(Filled),
    lists:sum([N || S := N <- Count, S /= $█]) - Ymin.

star2(Map) ->
    Limits = tools:min_max_grid(Map),
    Filled = fill([{500, 1}], [], Limits, Map#{{500, 1} => $|, {500, 0} => $+}),
    tools:count($~, Filled).

read(File) ->
    Points = tools:read_lines(File, fun parse_clay/1),
    maps:from_keys(lists:flatten(Points), $█).

parse_clay("x=" ++ Rest) ->
    [X, Ymin, Ymax] = tools:parse_format(Rest, "~d, y=~d..~d"),
    [{X, Y} || Y <- lists:seq(Ymin, Ymax)];
parse_clay("y=" ++ Rest) ->
    [Y, Xmin, Xmax] = tools:parse_format(Rest, "~d, x=~d..~d"),
    [{X, Y} || X <- lists:seq(Xmin, Xmax)].

fill([], [], _Limits, Map) ->
    Map;
fill([], Next, Limits, Map) ->
    fill(Next, [], Limits, Map);
fill([{_X, Y} = Pos | Rest], Next, {_, {_, Ymax}} = Limits, Map) when Y > Ymax ->
    fill(Rest, Next, Limits, maps:remove(Pos, Map));
fill([Pos | Rest], Next, Limits, Map) ->
    case maps:get(Pos, Map) of
        $| ->
            {NextPos, NewStates} = check_flowing(Pos, Map);
        $~ ->
            {NextPos, NewStates} = check_still(Pos, Map)
    end,

    fill(Rest, Next ++ NextPos, Limits, maps:merge(Map, maps:from_list(NewStates))).

check_still({X, Y}, Map) ->
    {[P || P <- [{X, Y - 1}], maps:get(P, Map, $~) == $|], []}.

check_flowing({X, Y}, Map) ->
    case maps:get({X, Y + 1}, Map, $.) of
        $. ->
            {[{X, Y + 1}], [{{X, Y + 1}, $|}]};
        _ ->
            Left = check_left({X, Y}, Map),
            Right = check_right({X, Y}, Map),

            case {Left, Right} of
                {{wall, L}, {wall, R}} ->
                    {
                        [{Xn, Y} || Xn <- lists:seq(L, R)],
                        [{{Xn, Y}, $~} || Xn <- lists:seq(L, R)]
                    };
                {{_, L}, {_, R}} ->
                    {
                        [{Xn, Y} || Xn <- [L, R], Xn /= X],
                        [{{Xn, Y}, $|} || Xn <- lists:seq(L, R), Xn /= X]
                    }
            end
    end.
check_left({X, Y}, Map) ->
    case {maps:get({X, Y + 1}, Map, $.), maps:get({X - 1, Y}, Map, $.)} of
        {$█, $.} -> check_left({X - 1, Y}, Map);
        {$~, $.} -> check_left({X - 1, Y}, Map);
        {$█, $|} -> check_left({X - 1, Y}, Map);
        {$~, $|} -> check_left({X - 1, Y}, Map);
        {$., _} -> {flow, X};
        {$|, _} -> {flow, X};
        {_, $|} -> {flow, X};
        {_, $█} -> {wall, X}
    end.
check_right({X, Y}, Map) ->
    case {maps:get({X, Y + 1}, Map, $.), maps:get({X + 1, Y}, Map, $.)} of
        {$█, $.} -> check_right({X + 1, Y}, Map);
        {$~, $.} -> check_right({X + 1, Y}, Map);
        {$█, $|} -> check_right({X + 1, Y}, Map);
        {$~, $|} -> check_right({X + 1, Y}, Map);
        {$., _} -> {flow, X};
        {$|, _} -> {flow, X};
        {_, $|} -> {flow, X};
        {_, $█} -> {wall, X}
    end.
