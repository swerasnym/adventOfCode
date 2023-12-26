-module(aoc2020_day14).
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
        problem => {2020, 14},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    {_Mask, Memory} = lists:foldl(fun process1/2, {{mask, 0, 0}, #{}}, Data),
    lists:sum(maps:values(Memory)).

star2(Data) ->
    {_Mask, Memory} = lists:foldl(fun process2/2, {{mask, 0, 0}, #{}}, Data),
    lists:sum(maps:values(Memory)).

read(File) ->
    [parse(Line) || Line <- tools:read_lines(File)].

parse("mask = " ++ Mask) ->
    And = list_to_integer(tools:replace(Mask, $X, $1), 2),
    Or = list_to_integer(tools:replace(Mask, $X, $0), 2),
    {mask, And, Or, Mask};
parse("mem[" ++ Rest) ->
    {Pos, "] = " ++ ValueS} = string:to_integer(Rest),
    {Value, []} = string:to_integer(ValueS),
    {mem, Pos, Value}.

process1({mask, _And, _Or, _} = Mask, {_, Mem}) ->
    {Mask, Mem};
process1({mem, Pos, Value}, {{mask, And, Or, _} = Mask, Mem}) ->
    {Mask, Mem#{Pos => Value band And bor Or}}.

process2({mask, _And, _Or, M} = Mask, {_, Mem}) ->
    {{Mask, generate_masks(M)}, Mem};
process2({mem, Pos, Value}, {{{mask, _And, Or, M}, OrList} = Mask, Mem}) ->
    AndMask = tools:replace(M, $0, $1),
    BaseMask = list_to_integer(tools:replace(AndMask, $X, $0), 2),
    BasePos = Pos band BaseMask bor Or,

    NewMap = lists:foldl(fun(OrI, Map) -> Map#{BasePos bor OrI => Value} end, Mem, OrList),
    {Mask, NewMap}.

generate_masks(Mask) ->
    OrMask = tools:replace(Mask, $1, $0),
    generate_masks([OrMask], []).

generate_masks([], Acc) ->
    Acc;
generate_masks([Mask | Masks], Acc) ->
    case tools:replace(Mask, $X, $1, 1) of
        Mask ->
            generate_masks(Masks, [list_to_integer(Mask, 2) | Acc]);
        Mask2 ->
            generate_masks([Mask2, tools:replace(Mask, $X, $0, 1) | Masks], Acc)
    end.
