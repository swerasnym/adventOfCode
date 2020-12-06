-module(day6).

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
    Anyone = [maps:size(count(lists:flatten(Group))) || Group <- Data],
    lists:sum(Anyone).

star2(Data) ->
    Evryone =
        [begin
             Count = count(lists:flatten(Group)),
             [K || {K, V} <- maps:to_list(Count), V == length(Group)]
         end
         || Group <- Data],
    length(lists:flatten(Evryone)).

read(File) ->
    {ok, Bin} = file:read_file(File),
    [string:split(
         string:trim(Group), "\n", all)
     || Group
            <- string:split(
                   string:trim(binary_to_list(Bin)), "\n\n", all)].

count(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end, #{}, List).
