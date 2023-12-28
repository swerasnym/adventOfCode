-module(aoc2022_day11).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 11}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    erlang:erase(),
    Blocks = tools:read_blocks(File, fun tools:parse_lines/1),
    Monkeys = maps:from_list([parse_monkey(M) || M <- Blocks]),
    erlang:put(ids, lists:sort(maps:keys(Monkeys))),
    erlang:put(lcm, tools:lcm([test(M) || M <- maps:values(Monkeys)])),
    Monkeys.

star1(Monkeys) ->
    End = lists:foldl(fun(_, M) -> process(M) end, Monkeys, lists:seq(1, 20)),
    [A, B | _] = tools:dsort([count(M) || M <- maps:values(End)]),
    A * B.

star2(Monkeys) ->
    End = lists:foldl(fun(_, M) -> process2(M) end, Monkeys, lists:seq(1, 10000)),
    [A, B | _] = tools:dsort([count(M) || M <- maps:values(End)]),
    A * B.

parse_monkey([
    "Monkey " ++ IdS,
    "  Starting items: " ++ ItemsS,
    "  Operation: new = " ++ New,
    "  Test: divisible by " ++ TestS,
    "    If true: throw to monkey " ++ TrueS,
    "    If false: throw to monkey " ++ FalseS
]) ->
    [Id] = tools:parse_integers(IdS, ":"),
    Items = tools:parse_integers(ItemsS, ", "),
    [Test] = tools:parse_integers(TestS),
    [True] = tools:parse_integers(TrueS),
    [False] = tools:parse_integers(FalseS),
    {Id, #{
        items => Items,
        new => New,
        test => Test,
        true => True,
        false => False,
        count => 0
    }}.

new("old * old", Old) ->
    Old * Old;
new("old + " ++ Ds, Old) ->
    [D] = tools:parse_integers(Ds),
    Old + D;
new("old * " ++ Ds, Old) ->
    [D] = tools:parse_integers(Ds),
    Old * D.

releaf(Worry) ->
    Worry div 3.

rems(Worry) ->
    Worry rem erlang:get(lcm).

process(Monkeys) ->
    lists:foldl(fun(I, M) -> process(I, M, fun releaf/1) end, Monkeys, erlang:get(ids)).

process2(Monkeys) ->
    lists:foldl(fun(I, M) -> process(I, M, fun rems/1) end, Monkeys, erlang:get(ids)).

count(#{count := C}) ->
    C.

test(#{test := T}) ->
    T.

process(Id, Monkeys, F) ->
    ThisM =
        #{
            items := Items,
            new := New,
            test := Test,
            true := TrueId,
            false := FalseId,
            count := C
        } =
        maps:get(Id, Monkeys),
    NewItems = [F(new(New, Old)) || Old <- Items],
    {ToTrue, ToFalse} = lists:partition(fun(I) -> I rem Test == 0 end, NewItems),

    TrueM = #{items := TrueItems} = maps:get(TrueId, Monkeys),
    FalseM = #{items := FalseItems} = maps:get(FalseId, Monkeys),

    Monkeys#{
        Id => ThisM#{items => [], count => C + length(Items)},
        TrueId => TrueM#{items => TrueItems ++ ToTrue},
        FalseId => FalseM#{items => FalseItems ++ ToFalse}
    }.
