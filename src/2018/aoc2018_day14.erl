-module(aoc2018_day14).
-behaviour(aoc_solution).

-compile({inline, [get_value/2, set_value/3]}).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, "9"}, star1, 5158916779},
        {{data, "5"}, star1, 0124515891},
        {{data, "18"}, star1, 9251071085},
        {{data, "2018"}, star1, 5941429882},
        {{data, "51589"}, star2, 9},
        {{data, "01245"}, star2, 5},
        {{data, "92510"}, star2, 18},
        {{data, "59414"}, star2, 2018}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 14},
        examples => Examples
    }).

-define(SIZE, 50_000_000).
run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Input) ->
    Steps = erlang:list_to_integer(Input),
    Ref = atomics:new(Steps + 40, []),
    set_value(Ref, 0, 3),
    set_value(Ref, 1, 7),
    iterate1(Ref, 0, 1, Steps + 20, 2),

    erlang:list_to_integer([get_value(Ref, Idx) + $0 || Idx <- lists:seq(Steps, Steps + 9)]).

star2(Input) ->
    Ref = atomics:new(?SIZE, []),
    set_value(Ref, 0, 3),
    set_value(Ref, 1, 7),
    Pattern = [D - $0 || D <- Input],
    Current = lists:duplicate(length(Input) - 2, 0) ++ [3, 7],

    iterate2(Ref, 0, 1, 2, Current, Pattern).

read(File) ->
    tools:read_string(File).

iterate1(Ref, Elf1, Elf2, Steps, Next) when Next < Steps - 2 ->
    R1 = get_value(Ref, Elf1),
    R2 = get_value(Ref, Elf2),
    case R1 + R2 of
        R when R < 10 ->
            set_value(Ref, Next, R),
            NewNext = Next + 1;
        RR ->
            set_value(Ref, Next, RR div 10),
            set_value(Ref, Next + 1, RR rem 10),
            NewNext = Next + 2
    end,
    iterate1(Ref, (Elf1 + R1 + 1) rem NewNext, (Elf2 + R2 + 1) rem NewNext, Steps, NewNext);
iterate1(_Ref, _Elf1, _Elf2, _Steps, _Next) ->
    ok.

iterate2(Ref, Elf1, Elf2, Next, [_, C1 | Current], Pattern) ->
    R1 = get_value(Ref, Elf1),
    R2 = get_value(Ref, Elf2),
    case R1 + R2 of
        R when R < 10 ->
            set_value(Ref, Next, R),
            NewCurrents = [[C1 | Current] ++ [R]],
            NewNext = Next + 1;
        RR ->
            N1 = RR div 10,
            N2 = RR rem 10,
            set_value(Ref, Next, N1),
            set_value(Ref, Next + 1, N2),
            NewCurrents = [Current ++ [N1, N2], [C1 | Current] ++ [N1]],
            NewNext = Next + 2
    end,

    case lists:member(Pattern, NewCurrents) of
        false ->
            iterate2(
                Ref,
                (Elf1 + R1 + 1) rem NewNext,
                (Elf2 + R2 + 1) rem NewNext,
                NewNext,
                hd(NewCurrents),
                Pattern
            );
        true when Pattern == hd(NewCurrents) ->
            NewNext - length(Pattern);
        true ->
            NewNext - length(Pattern) - 1
    end.

set_value(Ref, Idx, Value) ->
    atomics:put(Ref, Idx + 1, Value).

get_value(Ref, Idx) ->
    atomics:get(Ref, Idx + 1).
