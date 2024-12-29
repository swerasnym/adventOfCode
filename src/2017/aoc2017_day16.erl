-module(aoc2017_day16).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, star2/2, read/1]).

info() ->
    %% cSpell:ignore baedc ceadb
    Examples = [
        {"examples/2017/day16_ex.txt", {star1, <<"abcde">>}, "baedc"},
        {"examples/2017/day16_ex.txt", {star2, {<<"abcde">>, 2}}, "ceadb"},
        {"examples/2017/day16_ex.txt", {star2, {<<"abcde">>, 10000}}, "abcde"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2017, 16},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Moves) ->
    star1(Moves, <<"abcdefghijklmnop">>).

star1(Moves, Dancers) ->
    Dance = dance(Moves),
    binary_to_list(Dance(Dancers)).

star2(Moves) ->
    star2(Moves, {<<"abcdefghijklmnop">>, 1000000000}).
star2(Moves, {Dancers, Times}) ->
    Res = tools:repeat_with_memory(Times, dance(Moves), Dancers),
    binary_to_list(Res).

read(File) ->
    string:split(tools:read_string(File), ",", all).

dance(Moves) ->
    fun(Dancers) ->
        lists:foldl(fun move/2, Dancers, Moves)
    end.

move("s" ++ Amount, State) ->
    N = byte_size(State) - list_to_integer(Amount),
    <<F:N/binary, L/binary>> = State,
    <<L/binary, F/binary>>;
move([$p, A, $/, B], State) ->
    Fun = fun
        (C) when C == <<A>> -> <<B>>;
        (C) when C == <<B>> -> <<A>>
    end,
    binary:replace(State, [<<A>>, <<B>>], Fun, [global]);
move("x" ++ Pos, State) ->
    [A, B] = tools:parse_format(Pos, "~d/~d"),
    C1 = binary:at(State, A),
    C2 = binary:at(State, B),

    Fun = fun
        (C) when C == <<C1>> -> <<C2>>;
        (C) when C == <<C2>> -> <<C1>>
    end,
    binary:replace(State, [<<C1>>, <<C2>>], Fun, [global]).
