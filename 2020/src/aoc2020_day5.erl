-module(aoc2020_day5).

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
    [parse(Ticket) || Ticket <- tools:read_lines(File)].

parse(Ticket) ->
    BinaryString =
        tools:replace(Ticket,
                      #{$F => $0,
                        $B => $1,
                        $L => $0,
                        $R => $1}),
    list_to_integer(BinaryString, 2).

find_seat([A, B | _Rest]) when A + 2 == B ->
    A + 1;
find_seat([_ | Rest]) ->
    find_seat(Rest).
