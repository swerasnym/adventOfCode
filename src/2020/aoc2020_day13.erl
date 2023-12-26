-module(aoc2020_day13).
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
        problem => {2020, 13},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Time, Buses}) ->
    {DepTime, Id} = lists:min([{dep_time(Id, Time), Id} || Id <- Buses, Id /= x]),
    Id * (DepTime - Time).

star2({_Time, Buses}) ->
    Reminders =
        [{-Rem, Id} || {Id, Rem} <- lists:zip(Buses, lists:seq(0, length(Buses) - 1)), Id /= x],
    {Rem, _Mod} = tools:chinese_remainder(Reminders),
    Rem.

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
