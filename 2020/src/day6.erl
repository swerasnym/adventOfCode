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
    Anyone =
        [length(lists:usort(
                    lists:flatten(Group)))
         || Group <- Data],
    lists:sum(Anyone).

star2(Data) ->
    Evryone =
        [begin
             Count =
                 tools:count(
                     lists:flatten(Group)),
             [K || {K, V} <- maps:to_list(Count), V == length(Group)]
         end
         || Group <- Data],
    lists:flatlength(Evryone).

read(File) ->
    [tools:parse_lines(Group) || Group <- tools:read_blocks(File)].
