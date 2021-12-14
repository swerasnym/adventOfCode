-module(aoc2021_day14).

-export([run/2, profile/3]).

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

profile(Star, File, Times) ->
    Data = read(File),

    case Star of
        star1 ->
            profile(fun() -> star1(Data) end, Times);
        star2 ->
            profile(fun() -> star2(Data) end, Times);
        _ ->
            Star1 = profile(fun() -> star1(Data) end, Times),
            Star2 = profile(fun() -> star2(Data) end, Times),
            {Star1, Star2}
    end.

profile(F, Times) ->
    Expected = F(),
    Results =
        [begin
             {Time, Expected} = timer:tc(F),
             Time
         end
         || _ <- lists:seq(1, Times)],
    {Expected, lists:sum(Results) / Times / 1000}.

star1({Init, Map}) ->
    Counts = tools:count(insert_n(Map, Init, 10)),
    Counts2 = [V || {_, V} <- maps:to_list(Counts)],
    lists:max(Counts2) - lists:min(Counts2).

star2({Init, Map}) ->
    Pairs = maps:keys(Map),
    Counts = maps:from_list([{K, tools:count(K ++ V)} || {K, V} <- maps:to_list(Map)]),
    Counts1 = combind_n(Pairs, Map, Counts, 39),
    Counts2 = insert_c(Counts1, Init, #{}),
    Counts3 = [V || {_, V} <- maps:to_list(Counts2)],

    lists:max(Counts3) - lists:min(Counts3).

read(File) ->
    [First, Rest] = tools:read_blocks(File),

    {First, maps:from_list([list_to_tuple(L) || L <- tools:parse_format(Rest, "~s -> ~c")])}.

insert_n(_Map, Val, 0) ->
    Val;
insert_n(Map, Val, Count) ->
    insert_n(Map, insert(Map, Val, []), Count - 1).

insert(_Map, [A], Res) ->
    lists:reverse(Res, [A]);
insert(Map, [A, B | Rest], Res) ->
    [C] = maps:get([A, B], Map),
    insert(Map, [B | Rest], [C, A | Res]).

combind_n(_Pairs, _Map, Counts, 0) ->
    Counts;
combind_n(Pairs, Map, Counts, N) ->
    Counts1 = maps:from_list([{Pair, combind(Pair, Map, Counts)} || Pair <- Pairs]),
    combind_n(Pairs, Map, Counts1, N - 1).

combind([A, B] = Pair, Map, Counts) ->
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
