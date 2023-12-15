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

-export([run/1, run/2, run/3, run/4]).
-export([default_info/0]).

run(M) ->
    run(M, all, input, default_options()).

run(M, Opts) ->
    run(M, all, input, Opts).

run(M, all, FileOrData) ->
    run(M, all, FileOrData, default_options()).

run(M, all, FileOrData, Opts) ->
    #{all := All} = M:info(),
    run(M, All, FileOrData, Opts);
run(M, StarOrStars, input, Opts) ->
    #{problem := {Year, Day}} = M:info(),
    File = aoc_web:get_input_path(Year, Day),
    Data = M:read(File),
    run_data_print(M, StarOrStars, Data, Opts);
run(M, StarOrStars, {data, Data}, Opts) ->
    run_data_print(M, StarOrStars, Data, Opts);
run(M, StarOrStars, File, Opts) ->
    FilePath = filename:join(code:lib_dir(aoc), File),
    Data = M:read(FilePath),
    run_data_print(M, StarOrStars, Data, Opts).

run_data_print(M, StarOrStars, Data, #{print := true} = Opts) ->
    Res = run_data(M, StarOrStars, Data, Opts),
    print(Res),
    Res;
run_data_print(M, StarOrStars, Data, #{print := false} = Opts) ->
    run_data(M, StarOrStars, Data, Opts).

run_data(M, Stars, Data, Opts) when is_list(Stars) ->
    [{Star, run_data(M, Star, Data, Opts)} || Star <- Stars];
run_data(M, Star, Data, Opts) ->
    case maps:get(time, Opts, false) of
        true ->
            {Time, Res} = timer:tc(M, Star, [Data]),
            #{result => Res, time => Time};
        false ->
            #{result => M:Star(Data)}
    end.

default_info() ->
    #{
        all => [star1, star2],
        solved => [],
        examples => []
    }.

default_options() ->
    #{time => true, print => true}.

print([]) ->
    ok;
print([{Module, Star, Result} | Rest]) ->
    io:format("~p ~p: ", [Module, Star]),
    print(Result),
    print(Rest);
print([{Star, Result} | Rest]) ->
    io:format("~p: ", [Star]),
    print(Result),
    print(Rest);
print(#{result := Res, time := Time}) ->
    io:format("~p (~p ms)~n", [Res, Time / 1000.0]);
print(#{result := Res}) ->
    io:format("~p~n", [Res]).
