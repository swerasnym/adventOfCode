-module(aoc2018_day1).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Data = [+1, -2, +3, +1],

    Examples = [
        {{data, Data}, star1, 3},
        {{data, Data}, star2, 2}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 1},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(FrequencyShifts) ->
    lists:sum(FrequencyShifts).

star2(FrequencyShifts) ->
    find_repeat(0, FrequencyShifts, FrequencyShifts, #{}).

read(File) ->
    tools:read_integers(File).

find_repeat(Value, [], All, Map) ->
    find_repeat(Value, All, All, Map);
find_repeat(Value, [Change | Rest], All, Map) ->
    Next = Value + Change,
    case maps:is_key(Next, Map) of
        true ->
            Next;
        false ->
            find_repeat(Next, Rest, All, Map#{Next => true})
    end.
