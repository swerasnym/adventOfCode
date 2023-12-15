-module(aoc2023_day07).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2023, 7}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).
star1(Data) ->
    Ranked = lists:enumerate(lists:sort([value1(D) || D <- Data])),
    lists:sum([Rank * Bid || {Rank, {_, _, Bid}} <- Ranked]).

star2(Data) ->
    Ranked = lists:enumerate(lists:sort([value2(D) || D <- Data])),
    lists:sum([Rank * Bid || {Rank, {_, _, Bid}} <- Ranked]).

read(File) ->
    Lines = tools:read_lines(File, fun(L) -> string:split(L, " ") end),
    [{Cards, list_to_integer(Bid)} || [Cards, Bid] <- Lines].

parse_card1($T) ->
    10;
parse_card1($J) ->
    11;
parse_card1($Q) ->
    12;
parse_card1($K) ->
    13;
parse_card1($A) ->
    14;
parse_card1(C) when C >= $2, C =< $9 ->
    C - $0.
parse_card2($J) ->
    1;
parse_card2(C) ->
    parse_card1(C).

strength(Cards) ->
    Counts = tools:count(Cards),
    case (tools:dsort(maps:values(Counts))) of
        [5] ->
            10;
        [4, 1] ->
            9;
        [3, 2] ->
            8;
        [3, 1, 1] ->
            7;
        [2, 2, 1] ->
            6;
        [2, 1, 1, 1] ->
            5;
        [1, 1, 1, 1, 1] ->
            4
    end.

value1({Cards, Bid}) ->
    {strength(Cards), [parse_card1(C) || C <- Cards], Bid}.

value2({Cards, Bid}) ->
    {
        lists:max(
            [strength(lists:flatten(string:replace(Cards, "J", R, all))) || R <- Cards]
        ),
        [parse_card2(C) || C <- Cards],
        Bid
    }.
