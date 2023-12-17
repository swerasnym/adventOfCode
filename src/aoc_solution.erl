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

-export([run/0, run/1, run/2, run/3, run/4]).
-export([get_all_solutions/0]).
-export([default_info/0]).

run() ->
    SolutionModules = get_all_solutions(),
    Sorted = lists:sort([{maps:get(problem, M:info(), missing), M} || M <- SolutionModules]),
    {_, Modules} = lists:unzip(Sorted),
    run(Modules, all, both).

run(M) ->
    run(M, all, input, default_options()).

run(M, Opts) ->
    run(M, all, input, Opts).

run(M, StarOrStars, FileOrData) ->
    run(M, StarOrStars, FileOrData, default_options()).

run(M, StarOrStars, FileOrData, #{summary := true}) ->
    Results = run_file(M, StarOrStars, FileOrData),
    io:format("Summary:~n"),
    print(Results),
    Results;
run(M, StarOrStars, FileOrData, _Opts) ->
    run_file(M, StarOrStars, FileOrData).
% Modules
run_file(Modules, StarOrStars, FileOrData) when is_list(Modules) ->
    [run_file(M, StarOrStars, FileOrData) || M <- Modules];
run_file(M, StarOrStars, {files, Files}) when is_list(Files) ->
    [run_file(M, StarOrStars, File) || File <- Files, File /= both];
run_file(M, StarOrStars, {data, Data}) ->
    run_data(M, StarOrStars, Data);
run_file(M, StarOrStars, both) ->
    run_file(M, StarOrStars, {files, [examples, input]});
run_file(M, StarOrStars, input) ->
    #{problem := {Year, Day}} = M:info(),
    File = aoc_web:get_input_path(Year, Day),
    run_file(M, StarOrStars, File);
run_file(M, StarOrStars, examples) ->
    #{examples := Examples} = M:info(),
    [run_file(M, StarOrStars, {example, Example}) || Example <- Examples];
run_file(M, _, {example, {File, Star, ExpectedResult}}) ->
    validate_example(run_file(M, Star, File), ExpectedResult);
run_file(M, StarOrStars, File) when is_list(File) ->
    FilePath = filename:join(code:lib_dir(aoc), File),
    {Time, Data} = timer:tc(M, read, [FilePath]),
    [
        #{file => FilePath, time => Time, module => M},
        run_data(M, StarOrStars, Data)
    ].

run_data(M, all, Data) ->
    #{all := All} = M:info(),
    run_data(M, All, Data);
run_data(M, Stars, Data) when is_list(Stars) ->
    [run_data(M, Star, Data) || Star <- Stars];
run_data(M, Star, Data) ->
    io:format("~p:~p", [M, Star]),
    {Time, Res} = timer:tc(M, Star, [Data]),
    io:format(" -> ~p~n", [Res]),
    #{result => Res, time => Time, module => M, star => Star}.

default_info() ->
    #{
        all => [star1, star2],
        solved => [],
        examples => []
    }.

default_options() ->
    #{summary => true}.
print([]) ->
    ok;
print([R | Rest]) ->
    print(R),
    print(Rest);
print(#{file := File, time := Time}) ->
    io:format("~s (~p ms)~n", [File, Time / 1000.0]);
print(#{result := Res, test := ok, time := Time, module := M, star := Star}) ->
    io:format("~p:~p -> ~p (OK, ~p ms)~n", [M, Star, Res, Time / 1000.0]);
print(#{result := Res, test := {failed, Exp}, time := Time, module := M, star := Star}) ->
    io:format("~p:~p -> ~p (FAIL, ~p ms) expected:~p,~n", [M, Star, Res, Time / 1000.0, Exp]);
print(#{result := Res, time := Time, module := M, star := Star}) ->
    io:format("~p:~p -> ~p (~p ms)~n", [M, Star, Res, Time / 1000.0]).

validate_example([#{file := _} = File, Example], ExpectedResult) ->
    [File, validate_example(Example, ExpectedResult)];
validate_example(#{result := Res} = Result, ExpectedResult) ->
    case Res == ExpectedResult of
        true ->
            Result#{test => ok};
        false ->
            Result#{test => {failed, ExpectedResult}}
    end.

get_all_solutions() ->
    {ok, _} = application:ensure_all_started(aoc),
    {ok, aoc} = application:get_application(?MODULE),
    {ok, Modules} = application:get_key(aoc, modules),
    lists:filter(fun is_aoc_solution/1, Modules).

is_aoc_solution(Module) ->
    {module, Module} = code:ensure_loaded(Module),
    Attributes = erlang:get_module_info(Module, attributes),
    case proplists:get_value(behaviour, Attributes) of
        undefined ->
            false;
        List ->
            lists:member(aoc_solution, List)
    end.
