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
    find_error(Preamble, Values).

star2(Data) ->
    Result = star1(Data),
    Sequence = find_sequence(Result, Data),
    lists:min(Sequence) + lists:max(Sequence).

read(File) ->
    {ok, Bin} = file:read_file(File),
    [list_to_integer(Line)
     || Line
            <- string:split(
                   string:trim(binary_to_list(Bin)), "\n", all)].

find_error(Preamble = [_First | Rest], [Value | Values]) ->
    case length([A + B || A <- Preamble, B <- Preamble, A < B, A + B == Value]) of
        0 ->
            Value;
        _ ->
            find_error(Rest ++ [Value], Values)
    end.

find_sequence(Result, Data) ->
    find_sequence(Result, Data, [], 0).

find_sequence(Result, _Data, Sequence, Sum) when Sum == Result ->
    Sequence;
find_sequence(Result, Data, [First | NewSequence], Sum) when Sum > Result ->
    find_sequence(Result, Data, NewSequence, Sum - First);
find_sequence(Result, [First | NewData], Sequence, Sum) ->
    find_sequence(Result, NewData, Sequence ++ [First], Sum + First).
