-module(day9).

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
    {Preamble, Values} = lists:split(25, Data),
    check(Preamble, Values).

star2(Data) ->
    {Preamble, Values} = lists:split(25, Data),
    Result = check(Preamble, Values),
    Sequence = find_contiious(Result, Data),
    lists:min(Sequence) + lists:max(Sequence).

read(File) ->
    {ok, Bin} = file:read_file(File),
    [transform(Line)
     || Line
            <- string:split(
                   string:trim(binary_to_list(Bin)), "\n", all)].

transform(Line) ->
    list_to_integer(Line).

check(Preamble = [_First | Rest], [Value | Values]) ->
    case length([A + B || A <- Preamble, B <- Preamble, A + B == Value]) of
        0 ->
            Value;
        _ ->
            check(Rest ++ [Value], Values)
    end.

find_contiious(Result, Data) ->
    find_contiious(Result, Data, [], 0).

find_contiious(Result, Data, Sequence, Sum) when Sum == Result ->
    Sequence;
find_contiious(Result, Data, [First | Rest], Sum) when Sum > Result ->
    find_contiious(Result, Data, Rest, Sum - First);
find_contiious(Result, [First | Rest], Sequence, Sum) ->
    find_contiious(Result, Rest, Sequence ++ [First], Sum + First).
