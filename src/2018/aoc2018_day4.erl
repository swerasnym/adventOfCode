-module(aoc2018_day4).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2018/day4_ex.txt", star1, 240},
        {"examples/2018/day4_ex.txt", star2, 4455}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 4},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Events) ->
    Processed = process_events(Events),
    MinutesAsleep = [{length(Asleep), Guard} || Guard := Asleep <- Processed],
    {Total, Guard} = lists:max(MinutesAsleep),
    {Times, Minute} = most_sleepy_minute(maps:get(Guard, Processed)),
    io:format(
        "Guard #~p spent ~p minutes asleep; mostly at 00:~p where he was asleep ~p times!~n",
        [Guard, Total, Minute, Times]
    ),
    Guard * Minute.

star2(Events) ->
    Processed = process_events(Events),
    MostSleepyMinutes = [{most_sleepy_minute(Asleep), Guard} || Guard := Asleep <- Processed],
    {{Times, Minute}, Guard} = lists:max(MostSleepyMinutes),
    io:format(
        "Guard #~p was most often asleep at the same time; a total of ~p times at 00:~p!~n",
        [Guard, Times, Minute]
    ),

    Guard * Minute.

read(File) ->
    Lines = tools:read_lines(File, {fun tools:parse_line/3, ["[~d-~d-~d ~d:~d] ", rest]}),
    Events = [{{Y, M, D}, {H, Min}, Action} || {[Y, M, D, H, Min], Action} <- Lines],
    lists:sort(Events).

process_events(Events) ->
    process_events(Events, dummy, awake, #{}).

process_events([{_, {0, From}, "falls asleep"} | Rest], Guard, awake, Map) ->
    process_events(Rest, Guard, {sleep, From}, Map);
process_events([{_, {0, To}, "wakes up"} | Rest], Guard, {sleep, From}, Map) ->
    Asleep = maps:get(Guard, Map, []),
    process_events(Rest, Guard, awake, Map#{Guard => Asleep ++ lists:seq(From, To - 1)});
process_events([{_, _, "Guard" ++ _ = Begins} | Rest], _Guard, awake, Map) ->
    [NewGuard] = tools:parse_line(Begins, "Guard #~d begins shift"),
    process_events(Rest, NewGuard, awake, Map);
process_events([], _Guard, awake, Map) ->
    Map.

most_sleepy_minute(SleepTimes) ->
    lists:max([{Count, Minute} || Minute := Count <- tools:count(SleepTimes)]).
