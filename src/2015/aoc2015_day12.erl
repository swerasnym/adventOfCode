-module(aoc2015_day12).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, ~"{\"a\":{\"b\":4},\"c\":-1}"}, star1, 3},
        {{data, ~"[1,2,3]"}, star1, 6},
        {{data, ~"[1,2,3]"}, star2, 6},
        {{data, ~"[1,{\"c\":\"red\",\"b\":2},3]"}, star2, 4},
        {{data, ~"{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}"}, star2, 0},
        {{data, ~"[1,\"red\",5]"}, star2, 6}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2015, 12},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Bin) ->
    Decoded = json:decode(Bin),
    sum(Decoded, 0).
star2(Bin) ->
    Decoded = json:decode(Bin),
    sum_non_red(Decoded, 0).

read(File) ->
    {ok, Bin} = file:read_file(File),
    Bin.

sum(List, Sum) when is_list(List) ->
    lists:foldl(fun sum/2, Sum, List);
sum(Map, Sum) when is_map(Map) ->
    maps:fold(fun(Key, Val, Acc) -> sum(Key, 0) + sum(Val, Acc) end, Sum, Map);
sum(Val, Sum) when is_integer(Val) ->
    Sum + Val;
sum(Val, Sum) when is_binary(Val) ->
    Sum.

sum_non_red(List, Sum) when is_list(List) ->
    lists:foldl(fun sum_non_red/2, Sum, List);
sum_non_red(Map, Sum) when is_map(Map) ->
    case lists:member(~"red", maps:values(Map)) of
        true ->
            Sum;
        false ->
            maps:fold(
                fun(Key, Val, Acc) -> sum_non_red(Key, 0) + sum_non_red(Val, Acc) end, Sum, Map
            )
    end;
sum_non_red(Val, Sum) when is_integer(Val) ->
    Sum + Val;
sum_non_red(Val, Sum) when is_binary(Val) ->
    Sum.
