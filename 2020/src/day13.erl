-module(day13).

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

star1({Time, Buses}) ->
    {DepTime, Id} = lists:min([{dep_time(Id, Time), Id} || Id <- Buses, Id /= x]),
    Id * (DepTime - Time).

star2({_Time, Buses}) ->
    Reminders =
        [{-Rem, Id} || {Id, Rem} <- lists:zip(Buses, lists:seq(0, length(Buses) - 1)), Id /= x],
    tools:chinese_remainder(Reminders).

dep_time(Id, Time) ->
    case Time div Id * Id of
        Time ->
            Time;
        Other ->
            Other + Id
    end.

parse("x") ->
    x;
parse(Number) ->
    list_to_integer(Number).

read(File) ->
    [Time, BussesStr] = tools:read_lines(File),
    {parse(Time), [parse(Bus) || Bus <- string:tokens(BussesStr, ",")]}.
