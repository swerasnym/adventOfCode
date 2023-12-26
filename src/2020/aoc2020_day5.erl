-module(aoc2020_day5).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2020/data/dayN_ex.txt", star1, unknown},
        % {"2020/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2020, 5},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).
star1(Data) ->
    lists:max(Data).

star2(Data) ->
    find_seat(lists:sort(Data)).

read(File) ->
    [parse(Ticket) || Ticket <- tools:read_lines(File)].

parse(Ticket) ->
    BinaryString =
        tools:replace(
            Ticket,
            #{
                $F => $0,
                $B => $1,
                $L => $0,
                $R => $1
            }
        ),
    list_to_integer(BinaryString, 2).

find_seat([A, B | _Rest]) when A + 2 == B ->
    A + 1;
find_seat([_ | Rest]) ->
    find_seat(Rest).
