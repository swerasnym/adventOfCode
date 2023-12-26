-module(aoc2021_day3).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 3},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Counts = count_bits(Data),
    G = list_to_integer(gamma(Counts, []), 2),
    E = list_to_integer(epsilon(Counts, []), 2),
    E * G.

star2(Data) ->
    Oxy = oxygen(Data, 1),
    CO2 = co2(Data, 1),
    list_to_integer(Oxy, 2) * list_to_integer(CO2, 2).

read(File) ->
    tools:read_lines(File).

count_bits(Values) ->
    Length = length(hd(Values)),
    [count_bits(Pos, Values, {0, 0}) || Pos <- lists:seq(1, Length)].

count_bits(Pos, [Val | Values], {Zeros, Ones}) ->
    case lists:nth(Pos, Val) of
        $0 ->
            count_bits(Pos, Values, {Zeros + 1, Ones});
        $1 ->
            count_bits(Pos, Values, {Zeros, Ones + 1})
    end;
count_bits(_Pos, [], Result) ->
    Result.

gamma([{Zeros, Ones} | Counts], Res) ->
    case Zeros > Ones of
        true ->
            gamma(Counts, [$0 | Res]);
        false ->
            gamma(Counts, [$1 | Res])
    end;
gamma([], Res) ->
    lists:reverse(Res).

epsilon([{Zeros, Ones} | Counts], Res) ->
    case Zeros > Ones of
        true ->
            epsilon(Counts, [$1 | Res]);
        false ->
            epsilon(Counts, [$0 | Res])
    end;
epsilon([], Res) ->
    lists:reverse(Res).

oxygen([Result], _Pos) ->
    Result;
oxygen(Values, Pos) ->
    {Zeros, Ones} = count_bits(Pos, Values, {0, 0}),
    Filter =
        case Zeros > Ones of
            true ->
                $0;
            false ->
                $1
        end,

    Left = lists:filter(fun(Elem) -> Filter == lists:nth(Pos, Elem) end, Values),
    oxygen(Left, Pos + 1).

co2([Result], _Pos) ->
    Result;
co2(Values, Pos) ->
    {Zeros, Ones} = count_bits(Pos, Values, {0, 0}),
    Filter =
        case Zeros =< Ones of
            true ->
                $0;
            false ->
                $1
        end,

    Left = lists:filter(fun(Elem) -> Filter == lists:nth(Pos, Elem) end, Values),
    co2(Left, Pos + 1).
