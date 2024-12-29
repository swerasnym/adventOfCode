-module(aoc2021_day23).
-behaviour(aoc_solution).
-hank([{unnecessary_function_arguments, [{amphipod, 2}]}]).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 23},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({{Start, Goal}, _}) ->
    erlang:erase(),
    bfs(gb_sets:singleton({0, remove_fluff(Start)}), remove_fluff(Goal)).

star2({_, {Start, Goal}}) ->
    erlang:erase(),
    bfs(gb_sets:singleton({0, remove_fluff(Start)}), remove_fluff(Goal)).

read(File) ->
    Goal =
        "#############\n"
        "#...........#\n"
        "###A#B#C#D###\n"
        "  #A#B#C#D#\n"
        "  #########",

    Goal2 =
        "#############\n"
        "#...........#\n"
        "###A#B#C#D###\n"
        "  #A#B#C#D#\n"
        "  #A#B#C#D#\n"
        "  #A#B#C#D#\n"
        "  #########",
    L4 = "  #D#C#B#A#",
    L5 = "  #D#B#A#C#",

    [L1, L2, L3, L6, L7] = tools:read_lines(File),

    Start2 = string:join([L1, L2, L3, L4, L5, L6, L7], "\n"),

    {{tools:read_grid(File), tools:parse_grid(Goal)}, {
        tools:parse_grid(Start2), tools:parse_grid(Goal2)
    }}.

remove_fluff(Map) ->
    maps:filter(
        fun
            (_, $#) ->
                false;
            (_, $\s) ->
                false;
            (_, _) ->
                true
        end,
        Map
    ).

starts(Map) ->
    maps:to_list(maps:filter(fun amphipod/2, Map)).

bfs(Set1, Goal) ->
    {Element, Set2} = gb_sets:take_smallest(Set1),
    case Element of
        {Cost, Goal} ->
            Cost;
        {Cost, Current} ->
            case erlang:get(Current) of
                undefined ->
                    erlang:put(Current, visited),
                    Moves = moves(Current, Goal),
                    Next =
                        [
                            {Cost + cost(Start, End, Type), Current#{Start => $., End => Type}}
                         || {Start, End, Type} <- Moves
                        ],
                    Set3 = gb_sets:union(Set2, gb_sets:from_list(Next)),

                    bfs(Set3, Goal);
                visited ->
                    bfs(Set2, Goal)
            end
    end.

moves(Map, Goal) ->
    Open = maps:filter(fun(_, V) -> V == $. end, Map),

    [
        {Start, End, Type}
     || {Start, Type} <- starts(Map),
        End <- single_moves(Start, Open),
        valid_space(End),
        type(Start) /= type(End),
        leave_room(Start, Type, Map, Goal),
        enter_room(End, Type, Map, Goal)
    ].

single_moves(_Pos, Map) when map_size(Map) == 0 ->
    [];
single_moves(Pos, Map) ->
    case erlang:get({moves, Pos, Map}) of
        undefined ->
            Moves = [N || N <- neighbours(Pos), maps:get(N, Map, $#) == $.],
            Filled = maps:without(Moves, Map),

            PossibleMoves = Moves ++ lists:append([single_moves(N, Filled) || N <- Moves]),
            Result = [M || M <- PossibleMoves],
            put({moves, Pos, Map}, Result),
            Result;
        Result ->
            Result
    end.

neighbours({X, Y}) ->
    [{X, Y - 1}, {X, Y + 1}, {X - 1, Y}, {X + 1, Y}].

amphipod(_, $A) ->
    true;
amphipod(_, $B) ->
    true;
amphipod(_, $C) ->
    true;
amphipod(_, $D) ->
    true;
amphipod(_, _) ->
    false.

type({_, 1}) ->
    hall;
type({_, _}) ->
    room.

enter_room({_X, 1}, _Type, _Map, _Goal) ->
    true;
enter_room({X, Y} = Pos, Type, Map, Goal) ->
    maps:get(Pos, Goal) == Type andalso check_room(Type, {X, Y + 1}, Map).

leave_room({_X, 1}, _Type, _Map, _Goal) ->
    true;
leave_room({X, Y} = Pos, Type, Map, Goal) ->
    maps:get(Pos, Goal) /= Type orelse not check_room(Type, {X, Y + 1}, Map).

check_room(Type, {X, Y}, Map) ->
    case maps:get({X, Y}, Map, $#) of
        $# ->
            true;
        Type ->
            check_room(Type, {X, Y + 1}, Map);
        _ ->
            false
    end.

valid_space({3, 1}) ->
    false;
valid_space({5, 1}) ->
    false;
valid_space({7, 1}) ->
    false;
valid_space({9, 1}) ->
    false;
valid_space({_, _}) ->
    true.

cost({X1, Y1}, {X2, Y2}, Type) ->
    (abs(X1 - X2) + abs(Y1 - Y2)) * c(Type).

c($A) ->
    1;
c($B) ->
    10;
c($C) ->
    100;
c($D) ->
    1000.
