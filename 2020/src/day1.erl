-module(day1).

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
    hd([Value1 * Value2
        || Value1 <- Data, Value2 <- Data, Value1 + Value2 == 2020, Value1 =< Value2]).

star2(Data) ->
    hd([Value1 * Value2 * Value3
        || Value1 <- Data,
           Value2 <- Data,
           Value3 <- Data,
           Value1 + Value2 + Value3 == 2020,
           Value1 =< Value2,
           Value2 =< Value3]).

read_data(Device) ->
    read_data(Device, []).

read_data(Device, Acc) ->
    case io:fread(Device, [], "~d") of
        eof ->
            lists:reverse(Acc);
        {ok, [D]} ->
            read_data(Device, [D | Acc]);
        {error, What} ->
            io:format("io:fread error: ~w~n", [What]),
            read_data(Device, Acc)
    end.
