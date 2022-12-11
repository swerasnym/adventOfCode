-module(aoc2022_day11).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day11.txt"),
    io:format("S1: ~p ~nS2: ~p ~n", [S1, S2]),
    Res.

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

read(File) ->
    tools:read_blocks(File, parse_lines).

star1(Blocks) ->
    Monkeys = maps:from_list([parse_monkey(M) || M <- Blocks]),
    End = lists:foldl(fun(_, M) -> process(M) end, Monkeys, lists:seq(1, 20)),
    [A, B | _] = tools:dsort([count(M) || M <- maps:values(End)]),
    A * B.

star2(Blocks) ->
    Monkeys = maps:from_list([parse_monkey(M) || M <- Blocks]),
    End = lists:foldl(fun(_, M) -> process2(M) end, Monkeys, lists:seq(1, 10000)),
    [A, B | _] = tools:dsort([count(M) || M <- maps:values(End)]),
    A * B.

parse_monkey(["Monkey " ++ Monkey,
              "  Starting items: " ++ Items,
              "  Operation: new = " ++ New,
              "  Test: divisible by " ++ TestS,
              "    If true: throw to monkey " ++ TrueS,
              "    If false: throw to monkey " ++ FalseS]) ->
    [[Id]] = tools:parse_format(Monkey, "~d:"),
    ItemIds = tools:parse_integers(Items, " ,"),
    %% New,
    [[Test]] = tools:parse_format(TestS, "~d"),
    [[True]] = tools:parse_format(TrueS, "~d"),
    [[False]] = tools:parse_format(FalseS, "~d"),
    {Id,
     #{id => Id,
       items => ItemIds,
       new => New,
       test => Test,
       true => True,
       false => False,
       count => 0}}.

new("old * old", Old) ->
    Old * Old;
new("old + " ++ Ds, Old) ->
    [[D]] = tools:parse_format(Ds, "~d"),
    Old + D;
new("old * " ++ Ds, Old) ->
    [[D]] = tools:parse_format(Ds, "~d"),
    Old * D.

releaf(Worry) ->
    Worry div 3.

rems(Worry) ->
    Worry rem (19 * 3 * 11 * 17 * 5 * 2 * 13 * 7).

process(Monkeys) ->
    lists:foldl(fun process/2, Monkeys, lists:sort(maps:keys(Monkeys))).

process2(Monkeys) ->
    lists:foldl(fun process2/2, Monkeys, lists:sort(maps:keys(Monkeys))).

count(#{count := C}) ->
    C.

process(Id, Monkeys) ->
    ThisM =
        #{id := Id,
          items := Items,
          new := New,
          test := Test,
          true := TrueId,
          false := FalseId,
          count := C} =
            maps:get(Id, Monkeys),
    NewItems = [releaf(new(New, Old)) || Old <- Items],
    {ToTrue, ToFalse} = lists:partition(fun(I) -> I rem Test == 0 end, NewItems),

    TrueM = #{id := TrueId, items := TrueItems} = maps:get(TrueId, Monkeys),
    FalseM = #{id := FalseId, items := FalseItems} = maps:get(FalseId, Monkeys),

    Monkeys#{Id => ThisM#{items => [], count => C + length(Items)},
             TrueId => TrueM#{items => TrueItems ++ ToTrue},
             FalseId => FalseM#{items => FalseItems ++ ToFalse}}.

process2(Id, Monkeys) ->
    ThisM =
        #{id := Id,
          items := Items,
          new := New,
          test := Test,
          true := TrueId,
          false := FalseId,
          count := C} =
            maps:get(Id, Monkeys),
    NewItems = [rems(new(New, Old)) || Old <- Items],
    {ToTrue, ToFalse} = lists:partition(fun(I) -> I rem Test == 0 end, NewItems),

    TrueM = #{id := TrueId, items := TrueItems} = maps:get(TrueId, Monkeys),
    FalseM = #{id := FalseId, items := FalseItems} = maps:get(FalseId, Monkeys),

    Monkeys#{Id => ThisM#{items => [], count => C + length(Items)},
             TrueId => TrueM#{items => TrueItems ++ ToTrue},
             FalseId => FalseM#{items => FalseItems ++ ToFalse}}.
