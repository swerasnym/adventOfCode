-module(aoc2015_day11).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, "abcdefgh"}, star1, "abcdffaa"},
        {{data, "ghijklmn"}, star1, "ghjaabcc"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 11},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Password) ->
    next_password(Password).

star2(Password) ->
    tools:repeat(2, fun next_password/1, Password).

read(File) ->
    tools:read_string(File).

next_password(P) ->
    I = increment_password(P),
    case straight(I) andalso no_confusing_letters(I) andalso count_double(I) > 1 of
        true ->
            I;
        false ->
            next_password(I)
    end.

increment_password(P) ->
    increment_password(lists:reverse(P), []).

increment_password([$z | Rest], Acc) ->
    increment_password(Rest, [$a | Acc]);
increment_password([L | Rest], Acc) ->
    lists:reverse([L + 1 | Rest], Acc).

straight([]) -> false;
straight([A, B, C | _]) when A + 1 == B, B + 1 == C -> true;
straight([_ | Rest]) -> straight(Rest).

no_confusing_letters([]) -> true;
no_confusing_letters([$i | _]) -> false;
no_confusing_letters([$l | _]) -> false;
no_confusing_letters([$o | _]) -> false;
no_confusing_letters([_ | Rest]) -> no_confusing_letters(Rest).

count_double(String) ->
    count_double(String, 0).

count_double([A, A | Rest], Count) ->
    count_double(Rest, Count + 1);
count_double([_A | Rest], Count) ->
    count_double(Rest, Count);
count_double(_, Count) ->
    Count.
