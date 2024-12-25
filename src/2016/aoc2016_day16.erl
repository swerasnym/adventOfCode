-module(aoc2016_day16).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

info() ->
    Examples = [
        {{data, "10000"}, {star1, 20}, "01100"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 16},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Init) ->
    star1(Init, 272).

star1(Init, Length) ->
    Flipped = list_to_tuple([flip(I) || I <- Init]),
    Mids = generate_mids(length(Init), Length, []),
    checksum(0, Length, {list_to_tuple(Init), Flipped}, Mids).

star2(Init) ->
    star1(Init, 35651584).

read(File) ->
    tools:read_string(File).

digit(N, Flip, [Mid | Rest], Start, Size) when N > Mid ->
    digit(2 * Mid - N, not Flip, Rest, Start, Size);
digit(N, false, [Mid | _], _, _) when N == Mid ->
    $0;
digit(N, true, [Mid | _], _, _) when N == Mid ->
    $1;
digit(N, Flip, [Mid | Rest], Start, Size) when N < Mid andalso N > Size ->
    digit(N, Flip, Rest, Start, Size);
digit(N, false, _, {Init, _}, _) ->
    erlang:element(N, Init);
digit(N, true, _, {_, Flipped}, _) ->
    erlang:element(N, Flipped).

flip($0) -> $1;
flip($1) -> $0.

generate_mids(N, Max, Mids) when N >= Max ->
    Mids;
generate_mids(N, Max, Mids) when N < Max ->
    generate_mids(2 * N + 1, Max, [N + 1 | Mids]).

checksum(List) when length(List) rem 2 == 1 ->
    List;
checksum(List) ->
    checksum(List, []).

checksum([], Out) when length(Out) rem 2 == 1 ->
    lists:reverse(Out);
checksum([], Out) ->
    checksum(lists:reverse(Out), []);
checksum([A, A | Rest], Out) ->
    checksum(Rest, [$1 | Out]);
checksum([_, _ | Rest], Out) ->
    checksum(Rest, [$0 | Out]).

checksum(S, E, Init, Mids) when (E - S) rem 2 == 0 ->
    Mid = (S + E) div 2,
    checksum(checksum(S, Mid, Init, Mids) ++ checksum(Mid, E, Init, Mids));
checksum(S, E, {I, _} = Init, Mids) ->
    [digit(N, false, Mids, Init, tuple_size(I)) || N <- lists:seq(S + 1, E)].
