-module(aoc2024_day3).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {
            {data, "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"},
            star1,
            161
        },
        {
            {data, "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"},
            star2,
            48
        }
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 3},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Prefix = string:split(Data, "mul(", all),
    Split = [string:split(P, ")") || P <- Prefix],
    Matches = [
        tools:parse_integers(M, ",")
     || [M, _] <- Split, matches(M, "^[0-9]{1,3},[0-9]{1,3}$")
    ],
    lists:sum([A * B || [A, B] <- Matches]).

star2(Data) ->
    [E | DisabledEnabled] = string:split(Data, "don't()", all),
    DisabledEnabled2 = [string:split(DE, "do()") || DE <- DisabledEnabled],
    Enabled0 = [En || [_, En] <- DisabledEnabled2],
    Enabled = [E | Enabled0],
    io:format("~p~n", [length(Enabled)]),

    lists:sum([star1(En) || En <- Enabled]).

read(File) ->
    tools:read_string(File).

matches(String, Re) ->
    case re:run(String, Re) of
        {match, _} ->
            true;
        _ ->
            false
    end.
