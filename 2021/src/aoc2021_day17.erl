-module(aoc2021_day17).

-export([run/2, profile/3]).

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
        [
            begin
                {Time, Expected} = timer:tc(F),
                Time
            end
         || _ <- lists:seq(1, Times)
        ],
    {Expected, lists:sum(Results) / Times / 1000}.

star1([Xmin, Xmax, _Ymin, _Ymax] = Target) ->
    Vxmin = find_inital_vx_range(Xmin, 0),
    Vxmax = find_inital_vx_range(Xmax, 0),
    lists:max([
        step({0, 0}, {Vx, Vy}, Target, [])
     || Vx <- lists:seq(Vxmin - 1, Vxmax + 1), Vy <- lists:seq(0, 10000)
    ]).

star2([_Xmin, Xmax, Ymin, _Ymax] = Target) ->
    Steps =
        [
            {Vx, Vy, step({0, 0}, {Vx, Vy}, Target, [])}
         || Vx <- lists:seq(0, Xmax), Vy <- lists:seq(Ymin, 100)
        ],

    Hits = lists:filter(fun({_, _, Ym}) -> Ym >= 0 end, Steps),
    {length(Hits), Hits}.

read(File) ->
    [Tuple] = tools:read_format(File, "target area: x=~d..~d, y=~d..~d"),
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

%% find_inital_vx_range(X, 0) when X < 0 ->
%%     -find_inital_vx_range(-X, 0);
find_inital_vx_range(X, Vx) when X > 0 ->
    find_inital_vx_range(X - Vx - 1, Vx + 1);
find_inital_vx_range(_X, Vx) ->
    Vx.
