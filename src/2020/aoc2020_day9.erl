-module(aoc2020_day9).
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
        problem => {2020, 9},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    {Preamble, Values} = lists:split(25, Data),
    find_error(Preamble, Values).

star2(Data) ->
    Result = star1(Data),
    Sequence = find_sequence(Result, Data),
    lists:min(Sequence) + lists:max(Sequence).

read(File) ->
    tools:read_integers(File).

find_error([_ | Rest] = Preamble, [Value | Values]) ->
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
