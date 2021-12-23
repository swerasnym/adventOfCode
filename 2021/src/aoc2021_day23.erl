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
    eprof:analyze(total),
    eprof:stop(),
    Result.

star1({{Start, Goal}, _}) ->
    bfs([{0, Start}], Goal).

star2({_, {Start, Goal}}) ->
    bfs([{0, Start}], Goal).

read(File) ->
    erase(),

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

starts(Map) ->
    maps:to_list(
        maps:filter(fun amphipod/2, Map)).

bfs([{Cost, Goal} | _Rest], Goal) ->
    Cost;
bfs([{Cost, Current} | Rest], Goal) ->
    case get(Current) of
        undefined ->
            put(Current, visited),
            Moves = moves(Current, Goal),
            Next =
                [{Cost + cost(Start, End, Type), Current#{Start => $., End => Type}}
                 || {Start, End, Type} <- Moves],
            bfs(lists:umerge(Rest, lists:sort(Next)), Goal);
        visited ->
            bfs(Rest, Goal)
    end.

moves(Map, Goal) ->
    [{Start, End, Type}
     || {Start, Type} <- starts(Map),
        End <- single_moves(Start, Map),
        valid_space(End),
        type(Start) /= type(End),
        leave_room(Start, Type, Map, Goal),
        enter_room(End, Type, Map, Goal)].

single_moves(Pos, Map) ->
    Moves = [N || N <- neigbours(Pos), maps:get(N, Map, $#) == $.],
    Filled = maps:merge(Map, maps:from_keys(Moves, $!)),

    PossibleMoves = lists:flatten(Moves ++ [single_moves(N, Filled) || N <- Moves]),
    [M || M <- PossibleMoves].

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
    case maps:get({X, Y}, Map) of
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
