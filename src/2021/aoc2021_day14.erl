-module(aoc2021_day14).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 14},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Init, Map}) ->
    Counts = tools:count(insert_n(Map, Init, 10)),
    Result = maps:values(Counts),
    lists:max(Result) - lists:min(Result).

star2({Init, Map}) ->
    Pairs = maps:keys(Map),
    Counts0 = maps:from_list([{K, tools:count(K)} || K <- Pairs]),
    Counts40 = combine_n(Pairs, Map, Counts0, 40),
    Result = maps:values(insert_c(Counts40, Init, #{})),
    lists:max(Result) - lists:min(Result).

read(File) ->
    [First, Rest] = tools:read_blocks(File),

    {First,
        maps:from_list([list_to_tuple(L) || L <- tools:parse_multiple_formats(Rest, "~s -> ~c")])}.

insert_n(_Map, Val, 0) ->
    Val;
insert_n(Map, Val, Count) ->
    insert_n(Map, insert(Map, Val, []), Count - 1).

insert(_Map, [A], Res) ->
    lists:reverse(Res, [A]);
insert(Map, [A, B | Rest], Res) ->
    [C] = maps:get([A, B], Map),
    insert(Map, [B | Rest], [C, A | Res]).

combine_n(_Pairs, _Map, Counts, 0) ->
    Counts;
combine_n(Pairs, Map, Counts, N) ->
    Counts1 = maps:from_list([{Pair, combine(Pair, Map, Counts)} || Pair <- Pairs]),
    combine_n(Pairs, Map, Counts1, N - 1).

combine([A, B] = Pair, Map, Counts) ->
    [C] = maps:get(Pair, Map),
    M1 = add(maps:get([A, C], Counts), maps:get([C, B], Counts)),
    add(M1, #{C => -1}).

insert_c(_Counts, [A], Res) ->
    add(Res, #{A => 1});
insert_c(Counts, [A, B | Rest], Res) ->
    Count = maps:get([A, B], Counts),
    Res1 = add(Res, Count),
    insert_c(Counts, [B | Rest], add(Res1, #{B => -1})).

add(M1, M2) ->
    maps:merge_with(fun(_K, V1, V2) -> V1 + V2 end, M1, M2).
