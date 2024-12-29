-module(aoc2016_day21).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

%% cSpell:ignore decab fbgdceah
info() ->
    Examples = [
        {"examples/2016/day21_ex.txt", {star1, <<"abcde">>}, "decab"}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 21},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).
star1(Instructions) ->
    star1(Instructions, <<"abcdefgh">>).

star1(Instructions, Start) ->
    Res = lists:foldl(fun execute/2, Start, Instructions),
    binary_to_list(Res).

star2(Instructions) ->
    search(Instructions, tools:perms("abcdefgh")).

read(File) ->
    tools:read_lines(File, fun(L) -> string:split(L, " ", all) end).

search(Instructions, [P | Rest]) ->
    case star1(Instructions, list_to_binary(P)) of
        "fbgdceah" ->
            P;
        _ ->
            search(Instructions, Rest)
    end.

execute(["swap" | Rest], State) -> swap(Rest, State);
execute(["rotate" | Rest], State) -> rotate(Rest, State);
execute(["reverse" | Rest], State) -> reverse(Rest, State);
execute(["move" | Rest], State) -> move(Rest, State).

swap(["letter", [X], "with", "letter", [Y]], State) ->
    swap(X, Y, State);
swap(["position", Xs, "with", "position", Ys], State) ->
    X = list_to_integer(Xs),
    Y = list_to_integer(Ys),
    swap(binary:at(State, X), binary:at(State, Y), State).

swap(C1, C2, Binary) ->
    Fun = fun
        (C) when C == <<C1>> -> <<C2>>;
        (C) when C == <<C2>> -> <<C1>>
    end,
    binary:replace(Binary, [<<C1>>, <<C2>>], Fun, [global]).

rotate(["based", "on", "position", "of", "letter", [X]], State) ->
    [{Pos, 1}] = binary:matches(State, [<<X>>]),
    D =
        case Pos >= 4 of
            true -> byte_size(State) - tools:mod(Pos + 2, byte_size(State));
            false -> byte_size(State) - tools:mod(Pos + 1, byte_size(State))
        end,
    Head = binary:part(State, 0, D),
    Tail = binary:part(State, D, byte_size(State) - D),
    <<Tail/binary, Head/binary>>;
rotate(["right", Ds, "step" ++ _], State) ->
    D = byte_size(State) - list_to_integer(Ds),
    Head = binary:part(State, 0, D),
    Tail = binary:part(State, D, byte_size(State) - D),
    <<Tail/binary, Head/binary>>;
rotate(["left", Ds, "step" ++ _], State) ->
    D = list_to_integer(Ds),
    Head = binary:part(State, 0, D),
    Tail = binary:part(State, D, byte_size(State) - D),
    <<Tail/binary, Head/binary>>.

reverse(["positions", Xs, "through", Ys], State) ->
    X = list_to_integer(Xs),
    Y = list_to_integer(Ys),
    Part = binary:part(State, X, Y - X + 1),
    Reversed = list_to_binary(lists:reverse(binary_to_list(Part))),
    binary:replace(State, [Part], Reversed).

move(["position", Xs, "to", "position", Ys], State) ->
    X = list_to_integer(Xs),
    Y = list_to_integer(Ys),
    C1 = binary:at(State, X),
    C2 = binary:at(State, Y),

    Fun = fun
        (C) when C == <<C1>> -> <<>>;
        (C) when C == <<C2>>, Y > X -> <<C2, C1>>;
        (C) when C == <<C2>>, X > Y -> <<C1, C2>>
    end,
    binary:replace(State, [<<C1>>, <<C2>>], Fun, [global]).
