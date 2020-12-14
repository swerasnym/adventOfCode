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
    {Mask, Memory} = lists:foldl(fun process/2, {{mask, 0,0} ,#{}}, Data),
    lists:sum(maps:values(Memory)).

star2(Data) ->
   {Mask, Memory} = lists:foldl(fun process2/2, {{mask, 0,0} ,#{}}, Data),
    lists:sum(maps:values(Memory)).


read(File) ->
    {ok, Bin} = file:read_file(File),
    [ split(Line)
      %% list_to_integer(Line)
     || Line
            <- string:split(
                   string:trim(binary_to_list(Bin)), "\n", all)].


split("mask = "++Mask) ->
    And = list_to_integer(lists:flatten(string:replace(Mask,"X",$1,all)),2),
    Or = list_to_integer(lists:flatten(string:replace(Mask,"X",$0,all)),2),

    io:format("~s~n~.2B~n~.2B~n", [Mask, And, Or]),
    {mask, And, Or};
split("mem["++Rest) ->
    {Pos, "] = "++ValueS} = string:to_integer(Rest),
    {Value, []} = string:to_integer(ValueS),
    {mem, Pos, Value}.

    

process({mask, _And, _Or} = Mask, {_, Mem}) ->
    {Mask, Mem};
process({mem, Pos, Value}, {{mask, And, Or} = Mask, Mem}) ->
    {Mask,  Mem#{Pos=> (Value band And) bor Or}}.

process2({mask, _And, _Or} = Mask, {_, Mem}) ->
    {Mask, Mem};
process2({mem, Pos, Value}, {{mask, And, Or} = Mask, Mem}) ->
    {Mask,  Mem#{Pos=> (Value band And) bor Or}}.
