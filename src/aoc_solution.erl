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

-export([run/1, run/3]).
-export([default_info/0]).

run(M) ->
    run(M, all, input).

run(M, all, FileOrData) ->
    #{all := All} = M:info(),
    run(M, All, FileOrData);
run(M, StarOrStars, input) ->
    #{problem := {Year, Day}} = M:info(),
    File = aoc_web:get_input_path(Year, Day),
    Data = M:read(File),
    run_data(M, StarOrStars, Data);
run(M, StarOrStars, {data, Data}) ->
    run_data(M, StarOrStars, Data);
run(M, StarOrStars, File) ->
    FilePath = filename:join(code:lib_dir(aoc), File),
    Data = M:read(FilePath),
    run_data(M, StarOrStars, Data).

run_data(M, Stars, Data) when is_list(Stars) ->
    Results = [run_data(M, Star, Data) || Star <- Stars],
    Out = lists:zip(Stars, Results),
    io:format("Results: ~p~n", [Out]),
    Out;
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
