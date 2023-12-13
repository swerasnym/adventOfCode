-module(aoc_solution).

-type day() :: 1..25.
-type year() :: 2015..2023.

-type info() :: #{problem := {year(), day()}, _ => _}.
-type data() :: term().
-type result() :: term().
-export_type([result/0]).

-callback info() -> info().
-callback read(File :: file:name_all()) -> Data :: data().
-callback star1(Data :: data()) -> result().
-callback star2(Data :: data()) -> result().

-export([run/1, run/2, run/3, run_data/3]).
-export([default_info/0]).

run(M) ->
    #{
        problem := {Year, Day},
        all := All
    } = maps:merge(default_info(), M:info()),
    File = aoc_web:get_input_path(Year, Day),
    Data = M:read(File),
    [run_data(M, Star, Data) || Star <- All].

run(M, Star) ->
    #{problem := {Year, Day}} = M:info(),
    File = aoc_web:get_input_path(Year, Day),
    run(M, Star, File).
run(M, all, File) ->
    #{all := All} = maps:merge(default_info(), M:info()),
    Data = M:read(File),
    [run_data(M, Star, Data) || Star <- All];
run(M, Star, File) ->
    Data = M:read(File),
    run_data(M, Star, Data).

run_data(M, Star, Data) ->
    Res = M:Star(Data),
    io:format("~p: ~p ~n", [Star, Res]),
    Res.

default_info() ->
    #{
        all => [star1, star2],
        solved => [],
        examples => []
    }.
