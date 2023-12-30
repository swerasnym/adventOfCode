-module(aoc2018_day9).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, {9, 25}}, star1, 32},
        {{data, {10, 1618}}, star1, 8317},
        {{data, {13, 7999}}, star1, 146373},
        {{data, {17, 1104}}, star1, 2764},
        {{data, {21, 6111}}, star1, 54718},
        {{data, {30, 5807}}, star1, 37305}
        % {"examples/2018/day9_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 9},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({NoPlayers, LastMarble}) ->
    Players = queue:from_list(lists:seq(1, NoPlayers)),
    Marbles = lists:seq(1, LastMarble),
    game(Players, Marbles, [0], #{}).

star2({NoPlayers, LastMarble}) ->
    star1({NoPlayers, LastMarble * 100}).

read(File) ->
    String = tools:read_string(File),
    erlang:list_to_tuple(
        tools:parse_format(String, "~d players; last marble is worth ~d points")
    ).

game(_, [], _, Score) ->
    lists:max(maps:values(Score));
game(Players0, [1 | Marbles], _, Score) ->
    {{value, P}, Players1} = queue:out(Players0),
    game(queue:in(P, Players1), Marbles, [0, 1], Score);
game(Players0, [2 | Marbles], _, Score) ->
    {{value, P}, Players1} = queue:out(Players0),
    Ring = queue:from_list([1, 0, 2]),
    game(queue:in(P, Players1), Marbles, Ring, Score);
game(Players0, [M | Marbles], Ring0, Score) ->
    {{value, P}, Players1} = queue:out(Players0),

    case M rem 23 of
        0 ->
            Ring1 = rotate(7, Ring0),
            {{value, K}, Ring2} = queue:out_r(Ring1),
            {{value, C}, Ring3} = queue:out(Ring2),
            Ring = queue:in(C, Ring3),
            PSscore = maps:get(P, Score, 0) + M + K,
            game(queue:in(P, Players1), Marbles, Ring, Score#{P => PSscore});
        _ ->
            {{value, L}, Ring1} = queue:out(Ring0),
            Ring2 = queue:in(L, Ring1),
            Ring3 = queue:in(M, Ring2),
            game(queue:in(P, Players1), Marbles, Ring3, Score)
    end.

rotate(0, Ring) ->
    Ring;
rotate(N, Ring0) ->
    {{value, M}, Ring1} = queue:out_r(Ring0),
    rotate(N - 1, queue:in_r(M, Ring1)).
