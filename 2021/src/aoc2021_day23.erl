-module(aoc2021_day23).

-export([run/2, profile/3, eprof/2]).

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

eprof(Star, File) ->
    eprof:start(),
    eprof:start_profiling([self()]),
    Result = run(Star, File),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop(),
    Result.

star1({{Start, Goal}, _}) ->
    erase(),
    bfs(gb_sets:singleton({0, remove_fluff(Start)}), remove_fluff(Goal)).

star2({_, {Start, Goal}}) ->
    erase(),
    bfs(gb_sets:singleton({0, remove_fluff(Start)}), remove_fluff(Goal)).

read(File) ->
    Goal = "#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #########",

    Goal2 =
        "#############
#...........#
###A#B#C#D###
  #A#B#C#D#
  #A#B#C#D#
  #A#B#C#D#
  #########",
    L4 = "  #D#C#B#A#",
    L5 = "  #D#B#A#C#",

    [L1, L2, L3, L6, L7] = tools:read_lines(File),

    Start2 = string:join([L1, L2, L3, L4, L5, L6, L7], "\n"),

    {{tools:read_grid(File), tools:parse_grid(Goal)},
     {tools:parse_grid(Start2), tools:parse_grid(Goal2)}}.

remove_fluff(Map) ->
    maps:filter(fun (_, $#) ->
                        false;
                    (_, $\s) ->
                        false;
                    (_, _) ->
                        true
                end,
                Map).

starts(Map) ->
    maps:to_list(maps:filter(fun amphipod/2, Map)).

bfs(Set1, Goal) ->
    {Element, Set2} = gb_sets:take_smallest(Set1),
    case Element of
        {Cost, Goal} ->
            Cost;
        {Cost, Current} ->
            case get(Current) of
                undefined ->
                    put(Current, visited),
                    Moves = moves(Current, Goal),
                    Next =
                        [{Cost + cost(Start, End, Type), Current#{Start => $., End => Type}}
                         || {Start, End, Type} <- Moves],
                    Set3 = gb_sets:union(Set2, gb_sets:from_list(Next)),

                    bfs(Set3, Goal);
                visited ->
                    bfs(Set2, Goal)
            end
    end.

moves(Map, Goal) ->
    Open = maps:filter(fun(_, V) -> V == $. end, Map),

    [{Start, End, Type}
     || {Start, Type} <- starts(Map),
        End <- single_moves(Start, Open),
        valid_space(End),
        type(Start) /= type(End),
        leave_room(Start, Type, Map, Goal),
        enter_room(End, Type, Map, Goal)].

single_moves(_Pos, Map) when map_size(Map) == 0 ->
    [];
single_moves(Pos, Map) ->
    case get({moves, Pos, Map}) of
        undefined ->
            Moves = [N || N <- neigbours(Pos), maps:get(N, Map, $#) == $.],
            Filled = maps:without(Moves, Map),

            PossibleMoves = Moves ++ lists:append([single_moves(N, Filled) || N <- Moves]),
            Result = [M || M <- PossibleMoves],
            put({moves, Pos, Map}, Result),
            Result;
        Result ->
            Result
    end.

neigbours({X, Y}) ->
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
enter_room(Pos = {X, Y}, Type, Map, Goal) ->
    maps:get(Pos, Goal) == Type andalso check_room(Type, {X, Y + 1}, Map).

leave_room({_X, 1}, _Type, _Map, _Goal) ->
    true;
leave_room(Pos = {X, Y}, Type, Map, Goal) ->
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
