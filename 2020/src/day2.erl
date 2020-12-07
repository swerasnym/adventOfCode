-module(day2).

-export([run/2]).

run(Star, File) ->
    {ok, Device} = file:open(File, [read]),
    Data = read_data(Device),
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
    length([Line || Line <- Data, check_pwd(Line) == ok]).

star2(Data) ->
    length([Line || Line <- Data, check_pwd2(Line) == ok]).

read_data(Device) ->
    read_data(Device, []).

read_data(Device, Acc) ->
    case io:fread(Device, [], "~d-~d ~c:~s") of
        eof ->
            lists:reverse(Acc);
        {ok, D} ->
            read_data(Device, [D | Acc]);
        {error, What} ->
            io:format("io:fread error: ~w~n", [What]),
            read_data(Device, Acc)
    end.

check_pwd([Min, Max, [Char], Password]) ->
    Len = length([P || P <- Password, P == Char]),
    if Len < Min ->
           nok;
       Len > Max ->
           nok;
       true ->
           ok
    end.

check_pwd2([Idx1, Idx2, [Char], Password]) ->
    Pos1 = lists:nth(Idx1, Password),
    Pos2 = lists:nth(Idx2, Password),

    if Pos1 == Char, Pos2 == Char ->
           nok;
       Pos1 == Char ->
           ok;
       Pos2 == Char ->
           ok;
       true ->
           nok
    end.
