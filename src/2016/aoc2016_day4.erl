-module(aoc2016_day4).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day4_ex.txt", star1, 1514}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 4},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Rooms) ->
    lists:sum([Sector || {Name, Sector, Checksum} <- Rooms, valid_checksum(Name, Checksum)]).

star2(Rooms) ->
    Decrypted = [
        {decrypt(Name, Sector, ""), Sector}
     || {Name, Sector, Checksum} <- Rooms, valid_checksum(Name, Checksum)
    ],
    proplists:get_value("northpole object storage", Decrypted).

read(File) ->
    tools:read_lines(File, fun parse_room/1).

parse_room(L) ->
    [Head, Checksum0] = string:split(L, "["),
    Checksum = string:replace(Checksum0, "]", ""),
    [Name, Sector] = string:split(Head, "-", trailing),
    {Name, list_to_integer(Sector), lists:flatten(Checksum)}.

valid_checksum(Name, Checksum) ->
    Counts = tools:count(Name),
    Sorted = lists:sort([{-C, L} || L := C <- Counts, L /= $-]),
    Checksum == [L || {_, L} <- lists:sublist(Sorted, length(Checksum))].

decrypt([], _, Acc) ->
    lists:reverse(Acc);
decrypt([$- | Rest], Id, Acc) ->
    decrypt(Rest, Id, [$\s | Acc]);
decrypt([L | Rest], Id, Acc) ->
    Letter = $a + (((L - $a) + Id) rem 26),
    decrypt(Rest, Id, [Letter | Acc]).
