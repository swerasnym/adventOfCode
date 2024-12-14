-module(aoc2015_day21).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 21}
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Boss) ->
    find_win(100, gear_sets(), Boss).

star2(Boss) ->
    Hp = 100,
    Losses = [
        {Cost, Items}
     || {{Cost, Dmg, Armor}, Items} <- gear_sets(), fight({Hp, Dmg, Armor}, Boss) == boss
    ],
    {WorstCost, WorstItems} = lists:max(Losses),
    io:format("Can still lose using ~p costing ~p~n", [WorstItems, WorstCost]),
    WorstCost.

read(File) ->
    [[Hp, Dmg, Arm]] = tools:read_multiple_formats(File, "Hit Points: ~d\nDamage: ~d\nArmor: ~d"),
    {Hp, Dmg, Arm}.

weapons() ->
    [
        {dagger, {8, 4, 0}},
        {shortsword, {10, 5, 0}},
        {warhammer, {25, 6, 0}},
        {longsword, {40, 7, 0}},
        {greataxe, {74, 8, 0}}
    ].

armor() ->
    [
        {unarmed, {0, 0, 0}},
        {leather, {13, 0, 1}},
        {chainmail, {31, 0, 2}},
        {splintmail, {53, 0, 3}},
        {bandedmail, {75, 0, 4}},
        {platemail, {102, 0, 5}}
    ].

rings() ->
    [
        {none, {0, 0, 0}},
        {damage1, {25, 1, 0}},
        {damage2, {50, 2, 0}},
        {damage3, {100, 3, 0}},
        {defense1, {20, 0, 1}},
        {defense2, {40, 0, 2}},
        {defense3, {80, 0, 3}}
    ].

gear_sets() ->
    All = [
        [W, A, R1, R2]
     || W <- weapons(),
        A <- armor(),
        R1 <- rings(),
        R2 <- rings(),
        R1 < R2 orelse R1 == {none, {0, 0, 0}}
    ],
    Combos = [
        lists:mapfoldl(fun({W, Stats}, Sum) -> {W, aoc_vector:add(Stats, Sum)} end, {0, 0, 0}, E)
     || E <- All
    ],
    lists:sort([{S, I} || {I, S} <- Combos]).

find_win(Hp, [{{Cost, Dmg, Armor}, Items} | Rest], Boss) ->
    case fight({Hp, Dmg, Armor}, Boss) of
        player ->
            io:format("Win using ~p costing ~p~n", [Items, Cost]),
            Cost;
        boss ->
            find_win(Hp, Rest, Boss)
    end.

fight({PlayerHp, PlayerDmg, PlayerArmor}, {BossHp, BossDmg, BossArmor}) ->
    case
        rounds(PlayerHp, damage(BossDmg, PlayerArmor)) <
            rounds(BossHp, damage(PlayerDmg, BossArmor))
    of
        true ->
            boss;
        false ->
            player
    end.

rounds(Hp, Damage) ->
    case {Hp div Damage, Hp rem Damage} of
        {R, 0} ->
            R;
        {R, _} ->
            R + 1
    end.

damage(Att, Def) ->
    case Att - Def of
        V when V < 1 ->
            1;
        V ->
            V
    end.
