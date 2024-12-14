-module(aoc2015_day22).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

-record(state, {
    boss_hp,
    player_hp = 50,
    mana = 500,
    boss_dmg,
    turn = player,
    shield = 0,
    poison = 0,
    recharge = 0,
    spell = none
}).

info() ->
    Examples = [
        {{data, #state{player_hp = 10, mana = 250, boss_hp = 13, boss_dmg = 8}}, star1, 226},
        {{data, #state{player_hp = 10, mana = 250, boss_hp = 14, boss_dmg = 8}}, star1, 641}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 22},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Start) ->
    {Mana, End, Visited} = aoc_graph:dijkstra(Start, fun is_end/1, fun turn/1),
    io:format("Path: ~p~n", [aoc_graph:get_path(End, Visited)]),
    Mana.

star2(Start) ->
    {Mana, End, Visited} = aoc_graph:dijkstra(Start, fun is_end/1, fun turn_hard/1),
    io:format("Path: ~p~n", [aoc_graph:get_path(End, Visited)]),
    Mana.

read(File) ->
    [[BossHp, BossDmg]] = tools:read_multiple_formats(File, "Hit Points: ~d\nDamage: ~d"),
    #state{boss_hp = BossHp, boss_dmg = BossDmg}.

armor(#state{boss_hp = BossHp}) when BossHp =< 0 ->
    win;
armor(#state{shield = 0}) ->
    0;
armor(_) ->
    7.

damage(_, win) ->
    0;
damage(Att, Def) ->
    case Att - Def of
        V when V < 1 ->
            1;
        V ->
            V
    end.

is_end(#state{boss_hp = BossHp}) ->
    BossHp =< 0.

apply_effects(S) ->
    recharge(poison(shield(S))).

recharge(S = #state{recharge = 0}) ->
    S;
recharge(S = #state{recharge = Recharge, mana = Mana}) ->
    S#state{recharge = Recharge - 1, mana = Mana + 101}.

poison(S = #state{poison = 0}) ->
    S;
poison(S = #state{poison = Poison, boss_hp = BossHp}) ->
    S#state{poison = Poison - 1, boss_hp = BossHp - 3}.

shield(S = #state{shield = 0}) ->
    S;
shield(S = #state{shield = Shield}) ->
    S#state{shield = Shield - 1}.

turn(S = #state{turn = boss, player_hp = PlayerHp, boss_dmg = BossDmg}) ->
    SE = apply_effects(S),
    [{0, SE#state{turn = player, player_hp = PlayerHp - damage(BossDmg, armor(SE)), spell = none}}];
turn(#state{player_hp = PlayerHp}) when PlayerHp =< 0 ->
    %% Player is dead, no neighbouring states
    [];
turn(S) ->
    SE = apply_effects(S),
    lists:flatten([use_missile(SE), use_drain(SE), use_shield(SE), use_poison(SE), use_recharge(SE)]).

turn_hard(S = #state{turn = boss, player_hp = PlayerHp, boss_dmg = BossDmg}) ->
    SE = apply_effects(S),
    [{0, SE#state{turn = player, player_hp = PlayerHp - damage(BossDmg, armor(SE)), spell = none}}];
turn_hard(#state{player_hp = PlayerHp}) when PlayerHp =< 1 ->
    %% Player is dead, no neighbouring states
    [];
turn_hard(S = #state{player_hp = PlayerHp}) ->
    SE0 = apply_effects(S),
    SE = SE0#state{player_hp = PlayerHp - 1},
    lists:flatten([use_missile(SE), use_drain(SE), use_shield(SE), use_poison(SE), use_recharge(SE)]).

use_missile(#state{mana = Mana}) when Mana < 53 ->
    [];
use_missile(S = #state{mana = Mana, boss_hp = BossHp}) ->
    [{53, S#state{turn = boss, mana = Mana - 53, boss_hp = BossHp - 4, spell = missile}}].

use_drain(#state{mana = Mana}) when Mana < 73 ->
    [];
use_drain(S = #state{mana = Mana, boss_hp = BossHp, player_hp = PlayerHp}) ->
    [
        {73, S#state{
            turn = boss,
            mana = Mana - 73,
            boss_hp = BossHp - 2,
            player_hp = PlayerHp + 2,
            spell = drain
        }}
    ].

use_shield(#state{mana = Mana}) when Mana < 113 ->
    [];
use_shield(#state{shield = Shield}) when Shield > 0 ->
    [];
use_shield(S = #state{mana = Mana}) ->
    [{113, S#state{turn = boss, mana = Mana - 113, shield = 6, spell = shield}}].

use_poison(#state{mana = Mana}) when Mana < 173 ->
    [];
use_poison(#state{poison = Poison}) when Poison > 0 ->
    [];
use_poison(S = #state{mana = Mana}) ->
    [{173, S#state{turn = boss, mana = Mana - 173, poison = 6, spell = poison}}].

use_recharge(#state{mana = Mana}) when Mana < 229 ->
    [];
use_recharge(#state{recharge = Recharge}) when Recharge > 0 ->
    [];
use_recharge(S = #state{mana = Mana}) ->
    [{229, S#state{turn = boss, mana = Mana - 229, recharge = 5, spell = recharge}}].
