-module(aoc2020_day23).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2020/data/dayN_ex.txt", star1, unknown},
        % {"2020/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2020, 23},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Cups) ->
    Max = lists:max(Cups),
    Moves = 100,
    Ref = atomics:new(Max, []),

    make_circle(Ref, Cups, hd(Cups)),
    Cup1 = move(Ref, Moves, hd(Cups), Max),

    result(Ref, Cup1, 0).

star2(Cups) ->
    Extra = lists:max(Cups) + 1,
    Max = 1000000,
    Ref = atomics:new(Max, []),
    Moves = 10000000,

    make_circle(Ref, Cups ++ lists:seq(Extra, Max), hd(Cups)),
    Cup1 = move(Ref, Moves, hd(Cups), Max),

    Cup1 * next(Ref, Cup1).

read(File) ->
    lists:map(fun(V) -> V - $0 end, tools:read_string(File)).

make_circle(Ref, [Elem], First) ->
    0 = insert(Ref, Elem, First);
make_circle(Ref, [Elem, Next | Rest], First) ->
    0 = insert(Ref, Elem, Next),
    make_circle(Ref, [Next | Rest], First).

move(Ref, 0, _Cup0, _Max) ->
    next(Ref, 1);
move(Ref, Times, Cup0, Max) ->
    Cup1 = next(Ref, Cup0),
    Cup2 = next(Ref, Cup1),
    Cup3 = next(Ref, Cup2),
    Dest = destination(Cup0 - 1, [Cup1, Cup2, Cup3], Max),
    Tail = insert(Ref, Dest, Cup1),
    Next = insert(Ref, Cup3, Tail),
    Cup1 = insert(Ref, Cup0, Next),
    move(Ref, Times - 1, Next, Max).

next(Ref, Current) ->
    atomics:get(Ref, Current).

insert(Ref, Pos, New) ->
    atomics:exchange(Ref, Pos, New).

destination(0, Pickup, Max) ->
    destination(Max, Pickup, Max);
destination(Dest, [Cup1, Cup2, Cup3], _Max) when
    Dest /= Cup1, Dest /= Cup2, Dest /= Cup3
->
    Dest;
destination(Dest, Pickup, Max) ->
    destination(Dest - 1, Pickup, Max).

result(_Ref, 1, Result) ->
    Result;
result(Ref, Cup, Result) ->
    result(Ref, next(Ref, Cup), Result * 10 + Cup).
