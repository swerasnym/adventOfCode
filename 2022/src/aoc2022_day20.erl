-module(aoc2022_day20).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    maps:merge(aoc_solution:default_info(), #{problem => {2022, 20}}).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

read(File) ->
    tools:read_integers(File).

star1(Data) ->
    result(mix(lists:enumerate(Data))).

star2(Data) ->
    result(tools:repeat(10, fun mix/1, lists:enumerate([D * 811589153 || D <- Data]))).

result(IdxMixed) ->
    {_, Mixed} = lists:unzip(IdxMixed),
    ZeroFirst = tools:rotatewhile(fun(X) -> X /= 0 end, Mixed),
    lists:sum([lists:nth(N rem length(IdxMixed) + 1, ZeroFirst) || N <- [1000, 2000, 3000]]).

mix(IdxList) ->
    tools:repeat(length(IdxList), fun mix/2, IdxList).

mix(Idx, Mixed) ->
    [Elem = {Idx, N} | Rest] = tools:rotatewhile(fun({X, _}) -> X /= Idx end, Mixed),
    [Elem | tools:rotate(N, Rest)].
