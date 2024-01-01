-module(aoc2018_day15).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

-record(tile, {type, symbol}).
-record(unit, {type, symbol, pos = {0, 0}, dmg = 3, hp = 200}).
-record(state, {map, units, moved = [], rounds = 0, killed = #{elf => 0, goblin => 0}}).

info() ->
    Examples = [
        {"examples/2018/day15_ex1.txt", star1, 27730},
        {"examples/2018/day15_ex2.txt", star1, 36334},
        {"examples/2018/day15_ex3.txt", star1, 39514},
        {"examples/2018/day15_ex4.txt", star1, 27755},
        {"examples/2018/day15_ex5.txt", star1, 28944},
        {"examples/2018/day15_ex6.txt", star1, 18740}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 15},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

% 183192 to high.
star1(Initial) ->
    print_state(Initial),
    EndState = simulate(Initial),
    print_state(EndState),
    Left = lists:sum([Hp || #unit{hp = Hp} <- EndState#state.units ++ EndState#state.moved]),
    io:format("~p ~p~n", [Left, EndState#state.rounds]),
    Left * EndState#state.rounds.

star2(Initial) ->
    print_state(Initial),
    EndState = simulate2(4, Initial),
    print_state(EndState),
    Left = lists:sum([Hp || #unit{hp = Hp} <- EndState#state.units ++ EndState#state.moved]),
    io:format("~p ~p~n", [Left, EndState#state.rounds]),
    Left * EndState#state.rounds.

read(File) ->
    Map0 = tools:read_grid(File, #{
        $E => #unit{type = elf, symbol = $E},
        $G => #unit{type = goblin, symbol = $G},
        $. => #tile{type = empty, symbol = $\s},
        $# => #tile{type = wall, symbol = $█}
    }),

    Units = [U#unit{pos = P} || P := #unit{} = U <- Map0],
    Map = maps:map(fun remove_units/2, Map0),
    #state{map = Map, units = lists:sort(fun cmp_units/2, Units)}.

remove_units(max, Max) ->
    Max;
remove_units(_P, #unit{}) ->
    #tile{type = empty, symbol = $\s};
remove_units(_P, #tile{} = T) ->
    T.

to_symbol(max, Max) ->
    Max;
to_symbol(_P, #tile{symbol = S}) ->
    S;
to_symbol(_P, #unit{symbol = S}) ->
    S.

print_state(#state{map = Map, units = Units, moved = Moved} = S) ->
    io:nl(),
    print_round(S),
    UnitMap = maps:from_list([{P, U} || #unit{pos = P} = U <- Units ++ Moved]),
    Combined = maps:merge(Map, UnitMap),
    {Xmax, Ymax} = maps:get(max, Map),
    Symbols = maps:map(fun to_symbol/2, Combined),
    [print_line(L, Xmax, Symbols, UnitMap) || L <- lists:seq(0, Ymax)],
    ok.

print_line(Y, Xmax, Map, UnitMap) ->
    Units = [{P, U} || {_, Yp} = P := U <- UnitMap, Yp == Y],
    Row = [maps:get({X, Y}, Map) || X <- lists:seq(0, Xmax)],
    io:format("~ts", [Row]),
    print_units(Units).

print_units([]) ->
    io:nl();
print_units(Units) ->
    io:format("    "),
    Sorted = lists:sort(Units),
    Formatted = [
        io_lib:format("~c(~p)", [Symbol, Hp])
     || {_, #unit{symbol = Symbol, hp = Hp}} <- Sorted
    ],
    io:format("~s~n", [lists:join(", ", Formatted)]).

print_round(#state{rounds = 0, moved = []}) ->
    io:format("Initially:~n");
print_round(#state{rounds = 0, units = []}) ->
    io:format("After ~p round:~n", [1]);
print_round(#state{rounds = N, units = []}) ->
    io:format("After ~p rounds:~n", [N + 1]);
print_round(#state{rounds = N, units = U}) ->
    io:format("With ~p units to move in ~p round:~n", [length(U), N + 1]).

simulate2(Dmg, Initial) ->
    io:format("Damage: ~p -> ", [Dmg]),
    UpgradedUnits = lists:map(fun(U) -> add_dmg(Dmg, U) end, Initial#state.units),
    case simulate(Initial#state{units = UpgradedUnits}) of
        #state{killed = #{elf := 0}} = S ->
            S;
        _ ->
            simulate2(Dmg + 1, Initial)
    end.

add_dmg(Dmg, #unit{type = elf} = U) ->
    U#unit{dmg = Dmg};
add_dmg(_Dmg, U) ->
    U.

simulate(#state{rounds = N, units = [], moved = Moved} = S) ->
    % print_state(S),
    simulate(S#state{rounds = N + 1, units = lists:sort(fun cmp_units/2, Moved), moved = []});
simulate(#state{units = [#unit{type = Type, dmg = Dmg} = U | Units]} = S) ->
    OtherUnits = Units ++ S#state.moved,
    MapWithOtherUnits = maps:merge(
        S#state.map, maps:from_list([{OU#unit.pos, OU} || OU <- OtherUnits])
    ),
    Enemies = lists:filter(fun(Unit) -> enemy(U, Unit) end, OtherUnits),
    case length(Enemies) of
        0 ->
            io:format("~p at ~p Declares Victory!~n", [Type, U#unit.pos]),
            S;
        _ ->
            %   io:format("~p at ~p takes the following actions: ~n", [Type, U#unit.pos]),
            InRange = lists:usort(
                lists:flatten([open(P, MapWithOtherUnits) || #unit{pos = P} <- Enemies])
            ),

            NewPos = move(U#unit.pos, InRange, MapWithOtherUnits),

            AttackableSpaces = open(NewPos, S#state.map),
            AttackableEnemies = [
                E
             || E <- Enemies, lists:member(E#unit.pos, AttackableSpaces)
            ],
            {NewUnits, NewMoved, NewKilled} = attack(
                Dmg, AttackableEnemies, Units, S#state.moved, S#state.killed
            ),

            simulate(S#state{
                units = NewUnits, moved = [U#unit{pos = NewPos} | NewMoved], killed = NewKilled
            })
    end.

move(Pos, [], _Map) ->
    Pos;
move(Pos, InRange, Map) ->
    case lists:member(Pos, InRange) of
        true ->
            Pos;
        false ->
            Starts = open(Pos, Map),
            case move([{S, S} || S <- Starts], [], #{}, InRange, Map, []) of
                none ->
                    Pos;
                NewPos ->
                    % io:format("- Moves to: ~p~n", [NewPos]),
                    NewPos
            end
    end.

move([], _Next, _Visited, _InRange, _Map, Goals) when length(Goals) > 0 ->
    {_, Move} = hd(lists:sort(fun cmp_goal/2, Goals)),
    Move;
move([], [], _Visited, _InRange, _Map, []) ->
    none;
move([], Next, Visited, InRange, Map, []) ->
    move(lists:flatten(lists:reverse(Next)), [], Visited, InRange, Map, []);
move([{Pos, _First} | Rest], Next, Visited, InRange, Map, Goal) when is_map_key(Pos, Visited) ->
    move(Rest, Next, Visited, InRange, Map, Goal);
move([{Pos, First} = Current | Rest], Next, Visited, InRange, Map, Goal) ->
    case lists:member(Pos, InRange) of
        true ->
            move(Rest, Next, Visited#{Pos => true}, InRange, Map, [Current | Goal]);
        false ->
            New = [{O, First} || O <- open(Pos, Map)],
            move(Rest, [New | Next], Visited#{Pos => true}, InRange, Map, Goal)
    end.

attack(_, [], Units, Moved, Killed) ->
    {Units, Moved, Killed};
attack(Dmg, AttackableEnemies, Units, Moved, Killed) ->
    Enemy = hd(lists:sort(fun cmp_hp/2, AttackableEnemies)),
    % io:format("- Attacks: ~p~n", [Enemy]),
    case Enemy#unit.hp - Dmg of
        N when N =< 0 ->
            Before = maps:get(Enemy#unit.type, Killed),

            {Units -- [Enemy], Moved -- [Enemy], Killed#{Enemy#unit.type := Before + 1}};
        NewHp ->
            {update(Units, Enemy, NewHp, []), update(Moved, Enemy, NewHp, []), Killed}
    end.

update([], _, _, Acc) ->
    lists:reverse(Acc);
update([Unit | Rest], Unit, NewHp, Acc) ->
    lists:reverse(Acc, [Unit#unit{hp = NewHp} | Rest]);
update([Other | Rest], Unit, NewHp, Acc) ->
    update(Rest, Unit, NewHp, [Other | Acc]).

cmp_units(#unit{pos = {X1, Y1}}, #unit{pos = {X2, Y2}}) ->
    {Y1, X1} < {Y2, X2}.

cmp_goal({P, {X1, Y1}}, {P, {X2, Y2}}) ->
    {Y1, X1} < {Y2, X2};
cmp_goal({{X1, Y1}, _}, {{X2, Y2}, _}) ->
    {Y1, X1} < {Y2, X2}.

cmp_hp(#unit{hp = HP} = Unit1, #unit{hp = HP} = Unit2) ->
    cmp_units(Unit1, Unit2);
cmp_hp(#unit{hp = HP1}, #unit{hp = HP2}) ->
    HP1 < HP2.

enemy(#unit{type = T1}, #unit{type = T2}) ->
    T1 /= T2.

neighbours({X, Y}) -> [{X, Y - 1}, {X - 1, Y}, {X + 1, Y}, {X, Y + 1}].

open(P, Map) ->
    [Ps || Ps <- neighbours(P), is_empty(Ps, Map)].

is_empty(P, Map) ->
    case maps:get(P, Map) of
        #tile{type = empty} ->
            true;
        _ ->
            false
    end.
