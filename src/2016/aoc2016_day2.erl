-module(aoc2016_day2).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day2_ex.txt", star1, 1985},
        {"examples/2016/day2_ex.txt", star2, "5DB3"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 2},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Instructions) ->
    list_to_integer(get_code("", {1, 1}, Instructions, key_map1())).

star2(Instructions) ->
    get_code("", {-2, 2}, Instructions, key_map2()).

read(File) ->
    tools:read_lines(File).

%% erlfmt:ignore Make the layout visible
key_map1() ->
    #{
        {0, 0} => $1, {1, 0} => $2, {2, 0} => $3,
        {0, 1} => $4, {1, 1} => $5, {2, 1} => $6,
        {0, 2} => $7, {1, 2} => $8, {2, 2} => $9
    }.

%% erlfmt:ignore Make the layout visible
key_map2() ->
    #{
                                      {0, 0} => $1,
                       {-1, 1} => $2, {0, 1} => $3, {1, 1} => $4,
        {-2, 2} => $5, {-1, 2} => $6, {0, 2} => $7, {1, 2} => $8, {2, 2} => $9,
                       {-1, 3} => $A, {0, 3} => $B, {1, 3} => $C,
                                      {0, 4} => $D
    }.

get_code(Code, _, [], _) ->
    lists:reverse(Code);
get_code(Code, Pos, [Line | Rest], KeyMap) ->
    {Pos1, Button} = button(Pos, Line, KeyMap),
    get_code([Button | Code], Pos1, Rest, KeyMap).

button(Pos, [], KeyMap) ->
    {Pos, maps:get(Pos, KeyMap)};
button(Pos, [D | Rest], KeyMap) ->
    Next = move(Pos, D),
    case maps:is_key(Next, KeyMap) of
        true ->
            button(Next, Rest, KeyMap);
        false ->
            button(Pos, Rest, KeyMap)
    end.

move(Pos, $U) ->
    aoc_vector:add(Pos, {0, -1});
move(Pos, $D) ->
    aoc_vector:add(Pos, {0, 1});
move(Pos, $R) ->
    aoc_vector:add(Pos, {1, 0});
move(Pos, $L) ->
    aoc_vector:add(Pos, {-1, 0}).
