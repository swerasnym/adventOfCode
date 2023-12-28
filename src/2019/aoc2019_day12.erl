-module(aoc2019_day12).
-behaviour(aoc_solution).

-record(moon, {pos = {0, 0, 0}, vel = {0, 0, 0}}).

-hank([{unnecessary_function_arguments, [{step, 2}]}]).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 12},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    {Energy, _} = lists:foldl(fun step/2, {0, Data}, lists:seq(1, 1000)),
    Energy.

star2(Data) ->
    X = step2(Data, x),
    Y = step2(Data, y),
    Z = step2(Data, z),
    lcm(X, lcm(Y, Z)).

read(File) ->
    Moons = tools:read_lines(File, fun get_moon/1),
    maps:from_list(lists:enumerate(Moons)).

get_moon(Line) ->
    [X, Y, Z] = tools:parse_format(Line, "<x=~d, y=~d, z=~d>"),
    #moon{pos = {X, Y, Z}}.

step(_Count, Value) ->
    step(Value).

step({_Energy, Data0}) ->
    Data1 = velocity(Data0),
    Data2 = position(Data1),
    {energy(Data2), Data2}.

step2(Data0, Axis) ->
    F = fun(_Key, Value) -> get_axis(Axis, Value) end,

    Data = maps:map(F, Data0),
    step2(0, Data, Data).

step2(Step, Data0, Initial) ->
    Data1 = velocity(Data0),
    Data2 = position(Data1),
    case Data2 of
        Initial ->
            Step + 1;
        _ ->
            step2(Step + 1, Data2, Initial)
    end.

velocity(Data) ->
    lists:foldl(fun velocity/2, Data, pairs()).

velocity({M1, M2}, Data) ->
    #moon{pos = {X1, Y1, Z1}, vel = {Vx, Vy, Vz}} = Moon = maps:get(M1, Data),
    #moon{pos = {X2, Y2, Z2}} = maps:get(M2, Data),
    Dx = velocity(X1, X2),
    Dy = velocity(Y1, Y2),
    Dz = velocity(Z1, Z2),
    Data#{M1 => Moon#moon{vel = {Vx + Dx, Vy + Dy, Vz + Dz}}};
velocity(X1, X2) ->
    case X1 - X2 of
        0 ->
            0;
        Diff when Diff > 0 ->
            -1;
        Diff when Diff < 0 ->
            1
    end.

position(Data) ->
    lists:foldl(fun position/2, Data, [1, 2, 3, 4]).

position(M, Data) ->
    Moon = maps:get(M, Data),
    #moon{pos = {X, Y, Z}, vel = {Dx, Dy, Dz}} = Moon,
    Data#{M => Moon#moon{pos = {X + Dx, Y + Dy, Z + Dz}}}.

get_axis(all, Moon) ->
    Moon;
get_axis(x, #moon{pos = {X, _Y, _Z}, vel = {Vx, _Vy, _Vz}}) ->
    #moon{pos = {X, 0, 0}, vel = {Vx, 0, 0}};
get_axis(y, #moon{pos = {_X, Y, _Z}, vel = {_Vx, Vy, _Vz}}) ->
    #moon{pos = {0, Y, 0}, vel = {0, Vy, 0}};
get_axis(z, #moon{pos = {_X, _Y, Z}, vel = {_Vx, _Vy, Vz}}) ->
    #moon{pos = {0, 0, Z}, vel = {0, 0, Vz}}.

pairs() ->
    List = [1, 2, 3, 4],
    [{X, Y} || X <- List, Y <- List -- [X]].

energy(Data) when is_map(Data) ->
    lists:sum(lists:map(fun energy/1, maps:values(Data)));
energy(#moon{pos = {X, Y, Z}, vel = {Vx, Vy, Vz}}) ->
    (abs(X) + abs(Y) + abs(Z)) * (abs(Vx) + abs(Vy) + abs(Vz)).

gcd(A, 0) ->
    A;
gcd(A, B) ->
    gcd(B, A rem B).

lcm(A, B) ->
    A * B div gcd(A, B).
