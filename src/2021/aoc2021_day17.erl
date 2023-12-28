-module(aoc2021_day17).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 17},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1([Xmin, Xmax, _Ymin, _Ymax] = Target) ->
    VXmin = find_initial_vx_range(Xmin, 0),
    VXmax = find_initial_vx_range(Xmax, 0),
    lists:max([
        step({0, 0}, {Vx, Vy}, Target, [])
     || Vx <- lists:seq(VXmin - 1, VXmax + 1), Vy <- lists:seq(0, 10000)
    ]).

star2([_Xmin, Xmax, Ymin, _Ymax] = Target) ->
    Steps =
        [
            {Vx, Vy, step({0, 0}, {Vx, Vy}, Target, [])}
         || Vx <- lists:seq(0, Xmax), Vy <- lists:seq(Ymin, 100)
        ],

    Hits = lists:filter(fun({_, _, Ym}) -> Ym >= 0 end, Steps),
    length(Hits).

read(File) ->
    [Tuple] = tools:read_multiple_formats(File, "target area: x=~d..~d, y=~d..~d"),
    Tuple.

step({X, Y}, {_Vx, _Vy}, [Xmin, Xmax, Ymin, Ymax], Ys) when
    X >= Xmin, X =< Xmax, Y >= Ymin, Y =< Ymax
->
    lists:max([Y | Ys]);
step({_X, Y}, {_Vx, _Vy}, [_Xmin, _Xmax, Ymin, _Ymax], _Ys) when Y < Ymin ->
    -1;
step({X, Y}, {Vx, Vy}, Target, Ys) ->
    step({X + Vx, Y + Vy}, {u_vx(Vx), Vy - 1}, Target, [Y | Ys]).

u_vx(Vx) when Vx > 0 ->
    Vx - 1;
u_vx(Vx) when Vx < 0 ->
    Vx + 1;
u_vx(0) ->
    0.

find_initial_vx_range(X, Vx) when X > 0 ->
    find_initial_vx_range(X - Vx - 1, Vx + 1);
find_initial_vx_range(_X, Vx) ->
    Vx.
