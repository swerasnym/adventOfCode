-module(aoc2015_day5).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Data = ["ugknbfddgicrmopn", "aaa", "jchzalrnumimnmhp", "haegwjzuvuyypxyu", "dvszwmarrgswjxmb"],
    Data2 = [
        "qjhvhtzxzqqjkmpb",
        "xxyxx",
        "aaa",
        "uurcxstgmygtbstg",
        "ieodomkazucvgmuy",
        "xyx",
        "abcdefeghi",
        "xyxy",
        "aabcdefgaa",
        "aaaa"
    ],
    Examples = [
        {{data, Data}, star1, 2},
        {{data, Data2}, star2, 4}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 5},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Strings) ->
    Nice = [S || S <- Strings, not_naughty(S), repeated(S), three_vowels(S)],
    io:format("~p~n", [Nice]),
    length(Nice).

star2(Strings) ->
    Nice = [S || S <- Strings, repeated_pair(S), one_off(S)],
    io:format("~p~n", [Nice]),
    length(Nice).

read(File) ->
    tools:read_lines(File).

not_naughty([]) -> true;
not_naughty([_]) -> true;
not_naughty([$a, $b | _]) -> false;
not_naughty([$c, $d | _]) -> false;
not_naughty([$p, $q | _]) -> false;
not_naughty([$x, $y | _]) -> false;
not_naughty([_ | Rest]) -> not_naughty(Rest).

repeated([]) -> false;
repeated([_]) -> false;
repeated([A, A | _]) -> true;
repeated([_ | Rest]) -> repeated(Rest).

three_vowels(String) ->
    length([L || L <- String, vowel(L)]) >= 3.

vowel($a) -> true;
vowel($e) -> true;
vowel($i) -> true;
vowel($o) -> true;
vowel($u) -> true;
vowel(_) -> false.

one_off([_, _]) -> false;
one_off([A, _, A | _]) -> true;
one_off([_ | Rest]) -> one_off(Rest).

repeated_pair(String) ->
    Pairs = pairs(String, []),
    NonRepeating = remove_repeats(Pairs, []),
    Counts = tools:count(NonRepeating),
    lists:max(maps:values(Counts)) > 1.

remove_repeats([], Out) ->
    Out;
remove_repeats([A, A, A | Rest], Out) ->
    remove_repeats(Rest, [A, A | Out]);
remove_repeats([A, A | Rest], Out) ->
    remove_repeats([A | Rest], Out);
remove_repeats([A | Rest], Out) ->
    remove_repeats(Rest, [A | Out]).

pairs([_], Pairs) ->
    Pairs;
pairs([A, B | Rest], Pairs) ->
    pairs([B | Rest], [[A, B] | Pairs]).
