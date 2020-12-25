-module(day4).

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
    length([Passport || Passport <- Data, valid1(Passport)]).

star2(Data) ->
    length([Passport || Passport <- Data, valid1(Passport), valid2(Passport)]).

read(File) ->
    [to_map(tools:parse_format(Pass, " ~3a:~s")) || Pass <- tools:read_blocks(File)].

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
