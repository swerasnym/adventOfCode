-module(day5).

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
    lists:max(Data).

star2(Data) ->
    find_seat(lists:sort(Data)).

read(File) ->
    [to_int(Ticket) || Ticket <- tools:read_lines(File)].

to_int(Ticket) ->
    Substitute1 = string:replace(Ticket, "F", "0", all),
    Substitute2 = string:replace(Substitute1, "B", "1", all),
    Substitute3 = string:replace(Substitute2, "L", "0", all),
    Substitute4 = string:replace(Substitute3, "R", "1", all),
    list_to_integer(lists:flatten(Substitute4), 2).

find_seat([A, B | _Rest]) when A + 2 == B ->
    A + 1;
find_seat([_ | Rest]) ->
    find_seat(Rest).
