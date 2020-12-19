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
    maps:get(ok, count([validate(Passport) || Passport <- Data])).

star2(Data) ->
    maps:get(true, count([validate2(Passport) || Passport <- Data])).

read(File) ->
    {ok, Bin} = file:read_file(File),
    [to_map(string:split(
                string:replace(Pass, "\n", " ", all), " ", all))
     || Pass
            <- string:split(
                   string:trim(binary_to_list(Bin)), "\n\n", all)].

to_map(List) ->
    to_map(List, #{}).

to_map([], Map) ->
    Map;
to_map([Token | Rest], Map) ->
    [K, V] = string:split(Token, ":"),
    to_map(Rest, Map#{list_to_atom(K) => V}).

validate(#{byr := _Byr,
           iyr := _Iyr,
           eyr := _Eyr,
           hgt := _Hgt,
           hcl := _Hcl,
           ecl := _Ecl,
           pid := _Pid}) ->
    ok;
validate(_) ->
    nok.

validate2(#{byr := Byr,
            iyr := Iyr,
            eyr := Eyr,
            hgt := Hgt,
            hcl := Hcl,
            ecl := Ecl,
            pid := Pid}) ->
    byr(Byr) and eyr(Eyr) and iyr(Iyr) and hgt(Hgt) and hcl(Hcl) and ecl(Ecl) and pid_(Pid);
validate2(_) ->
    false.

is_int(S) ->
    try
        V = list_to_integer(S),
        {true, V}
    catch
        error:badarg ->
            false
    end.

byr(Byr) when length(Byr) == 4 ->
    case is_int(Byr) of
        {true, V} ->
            (V >= 1920) and (V =< 2002);
        _ ->
            false
    end;
byr(_) ->
    false.

iyr(Iyr) when length(Iyr) == 4 ->
    case is_int(Iyr) of
        {true, V} ->
            (V >= 2010) and (V =< 2020);
        _ ->
            false
    end;
iyr(_) ->
    false.

eyr(Eyr) when length(Eyr) == 4 ->
    case is_int(Eyr) of
        {true, V} ->
            (V >= 2020) and (V =< 2030);
        _ ->
            false
    end;
eyr(_) ->
    false.

hgt(Hgt = [_, _, _, $c, $m]) ->
    case is_int(string:substr(Hgt, 1, 3)) of
        {true, V} ->
            (V >= 150) and (V =< 193);
        _ ->
            false
    end;
hgt(Hgt = [_, _, $i, $n]) ->
    case is_int(string:substr(Hgt, 1, 2)) of
        {true, V} ->
            (V >= 59) and (V =< 76);
        _ ->
            false
    end;
hgt(_) ->
    false.

hcl(Hcl) ->
    case re:run(Hcl, "^#[0-9a-f]{6}$") of
        {match, _} ->
            true;
        _ ->
            false
    end.

ecl(Ecl) ->
    case re:run(Ecl, "^(amb|blu|brn|gry|grn|hzl|oth)") of
        {match, _} ->
            true;
        _ ->
            false
    end.

pid_(Pid) ->
    case re:run(Pid, "^[0-9]{9}$") of
        {match, _} ->
            true;
        _ ->
            false
    end.

count(List) ->
    Fun = fun(V) -> V + 1 end,
    lists:foldl(fun(Value, Map) -> maps:update_with(Value, Fun, 1, Map) end, #{}, List).
