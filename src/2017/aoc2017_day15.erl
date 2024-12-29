-module(aoc2017_day15).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, {65, 8921}}, star1, 588},
        {{data, {65, 8921}}, star2, 309}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 15},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Start) ->
    GenA = fun(In) -> gen(In, 16807) end,
    GenB = fun(In) -> gen(In, 48271) end,
    {Res, _} = tools:repeat(40_000_000, judge(GenA, GenB), {0, Start}),
    Res.

star2(Start) ->
    GenA = fun(In) -> gen(In, 16807, 3) end,
    GenB = fun(In) -> gen(In, 48271, 7) end,
    {Res, _} = tools:repeat(5_000_000, judge(GenA, GenB), {0, Start}),
    Res.

read(File) ->
    [[A, B]] = tools:read_multiple_formats(
        File, "Generator A starts with ~d\nGenerator B starts with ~d"
    ),
    {A, B}.

judge(GenA, GenB) ->
    fun({Count, {Ain, Bin}}) ->
        A = GenA(Ain),
        B = GenB(Bin),
        Check = (A bxor B) band 16#ffff,
        {tools:inc_on_true(Check == 0, Count), {A, B}}
    end.

gen(In, Mul) ->
    (In * Mul) rem 2147483647.

gen(In, Mul, And) ->
    Out = gen(In, Mul),
    case Out band And == 0 of
        true -> Out;
        false -> gen(Out, Mul, And)
    end.
