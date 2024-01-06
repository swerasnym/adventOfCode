-module(aoc2018_day24).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

-record(unit, {id, army, amount, hp, dmg, type, initiative, weakness = [], immunities = []}).

info() ->
    Examples = [
        {"examples/2018/day24_ex.txt", star1, 5216},
        {"examples/2018/day24_ex.txt", star2, 51}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 24},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Units) ->
    Survivors = simulate(Units),
    lists:sum([Amount || #unit{amount = Amount} <- Survivors]).

star2(Units) ->
    Survivors = search(Units),
    lists:sum([Amount || #unit{amount = Amount} <- Survivors]).

read(File) ->
    [["Immune System:" | ImmuneSystem], ["Infection:" | Infection]] = tools:read_blocks(
        File, fun tools:parse_lines/1
    ),
    C = atomics:new(1, []),

    Im = [parse_group(G, immune_system, C) || G <- ImmuneSystem],
    If = [parse_group(G, infection, C) || G <- Infection],
    Im ++ If.

parse_group(G, Army, C) ->
    {[Amount, Hp], Rest1} = tools:parse_format(G, "~d units each with ~d hit points ", rest),
    case Rest1 of
        "(" ++ WeakImmuneRest2 ->
            [WeakImmune, Rest2] = string:split(WeakImmuneRest2, ") ");
        _ ->
            WeakImmune = "",
            Rest2 = Rest1
    end,
    [Dmg, Type, Initiative] = tools:parse_format(
        Rest2, "with an attack that does ~d ~a damage at initiative ~d"
    ),
    {Weakness, Immunities} = parse_wi(WeakImmune),

    #unit{
        id = atomics:add_get(C, 1, 1),
        army = Army,
        amount = Amount,
        hp = Hp,
        dmg = Dmg,
        type = Type,
        initiative = Initiative,
        weakness = [list_to_atom(W) || W <- string:split(Weakness, ", "), W /= ""],
        immunities = [list_to_atom(I) || I <- string:split(Immunities, ", "), I /= ""]
    }.

parse_wi("") ->
    {[], []};
parse_wi(WeakImmune) ->
    case lists:sort(string:split(WeakImmune, "; ")) of
        ["weak to " ++ Weak] ->
            {Weak, []};
        ["immune to " ++ Immune] ->
            {[], Immune};
        ["immune to " ++ Immune, "weak to " ++ Weak] ->
            {Weak, Immune}
    end.

effective_power(#unit{amount = Amount, dmg = Dmg}) ->
    Amount * Dmg.

ep_cmp(#unit{initiative = IA} = A, #unit{initiative = IB} = B) ->
    {effective_power(A), IA} > {effective_power(B), IB}.

in_cmp(#unit{initiative = IA}, #unit{initiative = IB}) ->
    IA > IB.

enemy(#unit{army = A1}, #unit{army = A2}) ->
    A1 /= A2.

damage(#unit{amount = Amount, dmg = Dmg, type = Type}, #unit{weakness = Ws, immunities = Is}) when
    Amount > 0
->
    case {lists:member(Type, Ws), lists:member(Type, Is)} of
        {true, false} ->
            2 * Amount * Dmg;
        {false, true} ->
            0;
        {false, false} ->
            Amount * Dmg
    end;
damage(#unit{}, #unit{}) ->
    0.

target_selection(Units) ->
    EpSorted = lists:sort(fun ep_cmp/2, Units),
    target_selection(EpSorted, Units, #{}, #{}).

selected(#unit{id = Id}, Selected) ->
    maps:is_key(Id, Selected).

selector(Attacker, #unit{id = Id, initiative = Initiative} = Defender) ->
    {damage(Attacker, Defender), effective_power(Defender), Initiative, Id}.

target_selection([], _Units, Selections, _Selected) ->
    Selections;
target_selection([#unit{id = Id, army = _Army} = Attacker | Rest], Units, Selections, Selected) ->
    Selectors = [
        selector(Attacker, D)
     || D <- Units, enemy(Attacker, D), not selected(D, Selected)
    ],
    case lists:max([{0, 0, 0, 0} | Selectors]) of
        {0, _, _, _} ->
            %    io:format("~p in ~p selected ~p~n", [Id, Army, "No Target"]),
            target_selection(Rest, Units, Selections#{Id => none}, Selected);
        {_, _, _, Target} ->
            %    io:format("~p in ~p selected ~p~n", [Id, Army, Target]),
            target_selection(Rest, Units, Selections#{Id => Target}, Selected#{Target => true})
    end.

attacking(Units, Targets) ->
    UnitMap = #{Id => Unit || #unit{id = Id} = Unit <- Units},
    Order = lists:sort(fun in_cmp/2, Units),
    attacking(Order, UnitMap, Targets).

attacking([], UnitMap, _Targets) ->
    UnitMap;
attacking([#unit{id = Id, army = _Army} | Rest], UnitMap, Targets) ->
    Attacker = maps:get(Id, UnitMap),
    case maps:get(Id, Targets) of
        none ->
            attacking(Rest, UnitMap, Targets);
        Target ->
            #unit{amount = Amount, hp = Hp, id = DId} = Defender = maps:get(Target, UnitMap),
            Damage = damage(Attacker, Defender),
            Killed = min(Damage div Hp, Amount),
            %    io:format("~p in ~p attacks ~p, killing ~p~n", [Id, Army, DId, Killed]),
            attacking(Rest, UnitMap#{DId := Defender#unit{amount = Amount - Killed}}, Targets)
    end.

simulate(Units) ->
    Targets = target_selection(Units),
    %   io:nl(),
    Left = attacking(Units, Targets),
    %    io:nl(),
    Survivors = [U || #unit{amount = Amount} = U <- maps:values(Left), Amount > 0],
    GroupsLeft = [Army || #unit{army = Army} <- Survivors],
    case maps:size(tools:count(GroupsLeft)) of
        2 when Survivors /= Units ->
            simulate(Survivors);
        2 ->
            [draw];
        1 ->
            Survivors
    end.

total_hp(#unit{amount = Amount, hp = Hp}) ->
    Amount * Hp.

overkill(Units) ->
    Smallest = lists:min([Amount || #unit{army = immune_system, amount = Amount} <- Units]),
    MaxHp = lists:max([total_hp(U) || #unit{army = infection} = U <- Units]),
    MaxHp div Smallest + 1.

boost(N, #unit{army = immune_system, dmg = Dmg} = U) ->
    U#unit{dmg = Dmg + N};
boost(_N, #unit{army = infection} = U) ->
    U.

search(Units) ->
    search(Units, 0, overkill(Units)).

search(Units, Boost, Boost) ->
    io:format("~p: Minimal~n", [Boost]),
    simulate([boost(Boost, U) || U <- Units]);
search(Units, Min, Max) ->
    Boost = (Min + Max) div 2,
    io:format("~p:", [Boost]),
    Result = simulate([boost(Boost, U) || U <- Units]),
    case hd(Result) of
        draw ->
            io:format(" Draw~n"),
            search(Units, Boost + 1, Max);
        #unit{army = immune_system} ->
            io:format(" Win~n"),
            search(Units, Min, Boost);
        #unit{army = infection} ->
            io:format(" Loss~n"),
            search(Units, Boost + 1, Max)
    end.
