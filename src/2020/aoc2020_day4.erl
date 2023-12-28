-module(aoc2020_day4).
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
        problem => {2020, 4},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    length([Passport || Passport <- Data, valid1(Passport)]).

star2(Data) ->
    length([Passport || Passport <- Data, valid1(Passport), valid2(Passport)]).

read(File) ->
    [to_map(tools:parse_multiple_formats(Pass, " ~3a:~s")) || Pass <- tools:read_blocks(File)].

to_map(List) ->
    to_map(List, #{}).

to_map([], Map) ->
    Map;
to_map([[K, V] | Rest], Map) ->
    to_map(Rest, Map#{K => V}).

valid1(Pass) ->
    lists:all(fun(Field) -> is_map_key(Field, Pass) end, [byr, iyr, eyr, hgt, hcl, ecl, pid]).

valid2(Pass) ->
    lists:all(fun({Field, Value}) -> validate(Field, Value) end, maps:to_list(Pass)).

matches(String, Re) ->
    case re:run(String, Re) of
        {match, _} ->
            true;
        _ ->
            false
    end.

in_range(String, Min, Max) ->
    case string:to_integer(String) of
        {Value, []} when Value >= Min, Value =< Max ->
            true;
        _ ->
            false
    end.

validate(byr, Byr) ->
    in_range(Byr, 1920, 2002);
validate(iyr, Iyr) ->
    in_range(Iyr, 2010, 2020);
validate(eyr, Eyr) ->
    in_range(Eyr, 2020, 2030);
validate(hgt, Hgt) ->
    case string:to_integer(Hgt) of
        {Value, "cm"} when Value >= 150, Value =< 193 ->
            true;
        {Value, "in"} when Value >= 59, Value =< 76 ->
            true;
        _ ->
            false
    end;
validate(hcl, Hcl) ->
    matches(Hcl, "^#[0-9a-f]{6}$");
validate(ecl, Ecl) ->
    matches(Ecl, "^(amb|blu|brn|gry|grn|hzl|oth)$");
validate(pid, Pid) ->
    matches(Pid, "^[0-9]{9}$");
validate(cid, _) ->
    true.
