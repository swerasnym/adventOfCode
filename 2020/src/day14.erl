-module(day14).

-export([run/2]).

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

star1(Data) ->
    {_Mask, Memory} = lists:foldl(fun process/2, {{mask, 0, 0}, #{}}, Data),
    lists:sum(
        maps:values(Memory)).

star2(Data) ->
    {_Mask, Memory} = lists:foldl(fun process2/2, {{mask, 0, 0}, #{}}, Data),
    lists:sum(
        maps:values(Memory)).

read(File) ->
    {ok, Bin} = file:read_file(File),
    [split(Line)
     %% list_to_integer(Line)
     || Line
            <- string:split(
                   string:trim(binary_to_list(Bin)), "\n", all)].

split("mask = " ++ Mask) ->
    And = list_to_integer(lists:flatten(
                              string:replace(Mask, "X", $1, all)),
                          2),
    Or = list_to_integer(lists:flatten(
                             string:replace(Mask, "X", $0, all)),
                         2),

    {mask, And, Or, Mask};
split("mem[" ++ Rest) ->
    {Pos, "] = " ++ ValueS} = string:to_integer(Rest),
    {Value, []} = string:to_integer(ValueS),
    {mem, Pos, Value}.

process({mask, _And, _Or, _} = Mask, {_, Mem}) ->
    {Mask, Mem};
process({mem, Pos, Value}, {{mask, And, Or, _} = Mask, Mem}) ->
    {Mask, Mem#{Pos => Value band And bor Or}}.

process2({mask, _And, _Or, M} = Mask, {_, Mem}) ->
    {{Mask, generate_masks(M)}, Mem};
process2({mem, Pos, Value}, {{{mask, _And, Or, M}, OrList} = Mask, Mem}) ->
    AndMask =
        lists:flatten(
            string:replace(M, "0", $1, all)),
    BaseMask =
        list_to_integer(lists:flatten(
                            string:replace(AndMask, "X", $0, all)),
                        2),

    BasePos = Pos band BaseMask bor Or,

    NewMap = lists:foldl(fun(OrI, Map) -> Map#{BasePos bor OrI => Value} end, Mem, OrList),
    {Mask, NewMap}.

generate_masks(Mask) ->
    OrMask =
        lists:flatten(
            string:replace(Mask, "1", $0, all)),
    generate_masks([OrMask], []).

generate_masks([], Acc) ->
    Acc;
generate_masks([Mask | Masks], Acc) ->
    case lists:flatten(
             string:replace(Mask, "X", $1))
    of
        Mask ->
            generate_masks(Masks, [list_to_integer(Mask, 2) | Acc]);
        Mask2 ->
            generate_masks([Mask2,
                            lists:flatten(
                                string:replace(Mask, "X", $0))
                            | Masks],
                           Acc)
    end.
