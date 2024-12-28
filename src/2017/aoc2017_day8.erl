-module(aoc2017_day8).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2017/day8_ex.txt", star1, 1},
        {"examples/2017/day8_ex.txt", star2, 10}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 8},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Instructions) ->
    {_, End} = lists:foldl(fun run_instruction/2, {0, #{}}, Instructions),
    lists:max(maps:values(End)).

star2(Instructions) ->
    {Max, _} = lists:foldl(fun run_instruction/2, {0, #{}}, Instructions),
    Max.

read(File) ->
    Raw = tools:read_multiple_formats(File, "~s ~a ~d if ~s ~s ~d"),
    [lists:split(3, R) || R <- Raw].

run_instruction({Mod, Cond}, {Max, Map}) ->
    case check(Cond, Map) of
        false ->
            {Max, Map};
        true ->
            New = mod(Mod, Map),
            {max(Max, New), Map#{hd(Mod) => New}}
    end.
mod([Reg, inc, Value], Map) -> maps:get(Reg, Map, 0) + Value;
mod([Reg, dec, Value], Map) -> maps:get(Reg, Map, 0) - Value.

check([Reg, "<", Value], Map) -> maps:get(Reg, Map, 0) < Value;
check([Reg, ">", Value], Map) -> maps:get(Reg, Map, 0) > Value;
check([Reg, "==", Value], Map) -> maps:get(Reg, Map, 0) == Value;
check([Reg, "!=", Value], Map) -> maps:get(Reg, Map, 0) /= Value;
check([Reg, "<=", Value], Map) -> maps:get(Reg, Map, 0) =< Value;
check([Reg, ">=", Value], Map) -> maps:get(Reg, Map, 0) >= Value.
