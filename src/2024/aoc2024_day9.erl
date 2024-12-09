-module(aoc2024_day9).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2024/day9_ex.txt", star1, 1928},
        {"examples/2024/day9_ex.txt", star2, 2858}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 9},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    {Free, Files} = lists:partition(fun({_, T, _, _}) -> T == free end, Data),
    Moved = lists:sort(move1(Free, lists:reverse(Files), [])),
    checksum(0, Moved, 0).

star2(Data) ->
    {Free, Files} = lists:partition(fun({_, T, _, _}) -> T == free end, Data),
    Moved = lists:sort(move2(Free, lists:reverse(Files), [])),
    checksum(0, Moved, 0).

read(File) ->
    clasify(file, [C - $0 || C <- tools:read_string(File)], [], 0, 0).

clasify(_, [], Acc, _, _) ->
    lists:reverse(Acc);
clasify(free, [V | Rest], Acc, N, Id) ->
    clasify(file, Rest, [{{N, 0}, free, V, none} | Acc], N + 1, Id);
clasify(file, [V | Rest], Acc, N, Id) ->
    clasify(free, Rest, [{{N, 0}, file, V, Id} | Acc], N + 1, Id + 1).

move1([{_, free, 0, _} | Rest], Files, Acc) ->
    move1(Rest, Files, Acc);
move1(
    [{{N, Ns} = NFree, free, Size, none} | FreeRest], [{NFile, file, FileSize, Id} | Rest], Acc
) when NFile > NFree ->
    case Size >= FileSize of
        true ->
            move1(
                [{{N, Ns + 1}, free, Size - FileSize, none} | FreeRest],
                Rest,
                [{NFree, file, FileSize, Id} | Acc]
            );
        false ->
            move1(
                FreeRest,
                [{NFile, file, FileSize - Size, Id} | Rest],
                [{NFree, file, Size, Id} | Acc]
            )
    end;
move1(Free, Files, Acc) ->
    Files ++ Acc ++ Free.

move2(Free, [], Acc) ->
    Acc ++ Free;
move2(Free, [{NFile, file, FileSize, Id} = File | Rest], Acc) ->
    case find_space(FileSize, Free, []) of
        {found, Pos, NewFree} when Pos < NFile ->
            move2(NewFree, Rest, [
                {NFile, free, FileSize, none},
                {Pos, file, FileSize, Id}
                | Acc
            ]);
        _ ->
            move2(Free, Rest, [File | Acc])
    end.

find_space(_, [], _) ->
    none;
find_space(Size, [{{N, Ns} = NFree, free, FreeSize, none} | FreeRest], Acc) when Size =< FreeSize ->
    {found, NFree, lists:reverse(Acc, [{{N, Ns + 1}, free, FreeSize - Size, none} | FreeRest])};
find_space(Size, [ToSmall | FreeRest], Acc) ->
    find_space(Size, FreeRest, [ToSmall | Acc]).

checksum(_, [], Sum) ->
    Sum;
checksum(Pos, [{_, _, 0, _} | Rest], Sum) ->
    checksum(Pos, Rest, Sum);
checksum(Pos, [{X, file, Size, Value} | Rest], Sum) when Size > 0 ->
    checksum(Pos + 1, [{X, file, Size - 1, Value} | Rest], Sum + Pos * Value);
checksum(Pos, [{_, free, Size, _} | Rest], Sum) when Size > 0 ->
    checksum(Pos + Size, Rest, Sum).
