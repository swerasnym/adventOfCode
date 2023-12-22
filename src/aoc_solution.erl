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
-callback star1(Data :: data(), Parameters :: any()) -> result().
-callback star2(Data :: data()) -> result().
-callback star2(Data :: data(), Parameters :: any()) -> result().

-optional_callbacks([star1/2, star2/2]).

-export([run/0, run/1, run/2, run/3, run/4]).
-export([get_all_solutions/0]).
-export([default_info/0]).
-export([vt100code/1]).

run() ->
    SolutionModules = get_all_solutions(),
    Sorted = lists:sort([{maps:get(problem, M:info(), missing), M} || M <- SolutionModules]),
    Released = [M || {{Year, Day}, M} <- Sorted, aoc_web:check_date(Year, Day) == ok],
    run(Released).

run(M) ->
    run(M, all, both, default_options()).

run(M, Opts) ->
    run(M, all, both, Opts).

run(M, StarOrStars, FileOrData) ->
    run(M, StarOrStars, FileOrData, default_options()).

run(M, StarOrStars, FileOrData, #{summary := true}) ->
    Results0 = merge_meta(run_file(M, StarOrStars, FileOrData), #{}),
    Results = check_answers(true, lists:flatten([Results0])),

    io:format("~n~n~n~n~nResults:~n------------------------------~n"),
    print(Results, #{}),
    io:format("------------------------------~n"),
    Results;
run(M, StarOrStars, FileOrData, _Opts) ->
    merge_meta(run_file(M, StarOrStars, FileOrData), #{}).

run_file(Modules, StarOrStars, FileOrData) when is_list(Modules) ->
    %% List of modules
    [{#{module => M}, run_file(M, StarOrStars, FileOrData)} || M <- Modules];
run_file(M, StarOrStars, {files, Files}) when is_list(Files) ->
    %% List of files
    [run_file(M, StarOrStars, File) || File <- Files, File /= both];
run_file(M, StarOrStars, {data, Data}) ->
    %% Direct Data
    {#{source => data, data => Data}, run_data(M, StarOrStars, Data)};
run_file(M, StarOrStars, both) ->
    %% Alas to run both examples and inputs
    run_file(M, StarOrStars, {files, [examples, input]});
run_file(M, StarOrStars, input) ->
    %% Fetch input file
    #{problem := {Year, Day}} = M:info(),
    case aoc_web:get_input_path(Year, Day) of
        {ok, File} ->
            {#{type => input, problem => {Year, Day}}, run_file(M, StarOrStars, File)};
        {error, Error} ->
            {result, #{module => M, error => Error}, error}
    end;
run_file(M, StarOrStars, examples) ->
    %% Fetch examples
    #{examples := Examples} = M:info(),
    [run_file(M, StarOrStars, {example, Example}) || Example <- Examples];
run_file(M, _, {example, {File, Star, ExpectedResult}}) ->
    {#{type => example, expected => ExpectedResult}, run_file(M, Star, File)};
run_file(M, StarOrStars, File) when is_list(File) ->
    FilePath = filename:join(code:lib_dir(aoc), File),
    case filelib:is_file(FilePath) of
        false ->
            {result, #{module => M, error => "File not found: " ++ FilePath}, error};
        true ->
            {Time, Data} = timer:tc(M, read, [FilePath]),
            {
                #{source => file, path => FilePath, read_time => Time},
                run_data(M, StarOrStars, Data)
            }
    end.

run_data(M, all, Data) ->
    #{all := All} = M:info(),
    run_data(M, All, Data);
run_data(M, Stars, Data) when is_list(Stars) ->
    [run_data(M, Star, Data) || Star <- Stars];
run_data(M, {Star, Parameters}, Data) ->
    io:format("~p:~p ->~n", [M, Star]),
    {Time, Res} = timer:tc(M, Star, [Data, Parameters]),
    io:format("~p~n", [Res]),
    {result, #{time => Time, star => Star, parameters => Parameters}, Res};
run_data(M, Star, Data) ->
    io:format("~p:~p ->~n", [M, Star]),
    {Time, Res} = timer:tc(M, Star, [Data]),
    io:format("~p~n", [Res]),
    {result, #{time => Time, star => Star}, Res}.

merge_meta({result, Meta1, Result}, Meta2) ->
    {Result, maps:merge(Meta2, Meta1)};
merge_meta(List, Meta) when is_list(List) ->
    [merge_meta(I, Meta) || I <- List];
merge_meta({Meta1, Item}, Meta2) ->
    merge_meta(Item, maps:merge(Meta2, Meta1)).

default_info() ->
    #{
        all => [star1, star2],
        solved => [],
        examples => []
    }.

default_options() ->
    #{summary => true}.

print([], PrevMeta) ->
    PrevMeta;
print([R | Rest], PrevMeta1) ->
    PrevMeta2 = print(R, PrevMeta1),
    print(Rest, PrevMeta2);
print({Result, Meta}, PrevMeta) ->
    print_input(Meta, PrevMeta),
    print_result(Result, Meta),
    Meta.

print_input(#{module := M, error := Error}, #{module := M, error := Error}) ->
    ok;
print_input(#{module := M, error := Error}, PrevMeta) ->
    case maps:is_key(error, PrevMeta) of
        true ->
            ok;
        false ->
            print_line(PrevMeta)
    end,
    case io_lib:deep_char_list(Error) of
        true ->
            io:format("~p error: ~s~n", [M, Error]);
        false ->
            io:format("~p error: ~p~n", [M, Error])
    end;
print_input(
    #{type := Type, source := data, data := Data},
    #{type := Type, source := data, data := Data}
) ->
    ok;
print_input(#{type := Type, source := data, data := Data}, PrevMeta) ->
    print_line(PrevMeta),
    io:format("~p: ~0P~n", [Type, Data, 6]);
print_input(
    #{type := Type, source := file, path := FilePath},
    #{type := Type, source := file, path := FilePath}
) ->
    ok;
print_input(#{type := Type, source := file, path := FilePath}, PrevMeta) ->
    print_line(PrevMeta),
    io:format("~p: ~s~n", [Type, FilePath]).

print_line(Map) when map_size(Map) == 0 ->
    ok;
print_line(_) ->
    io:nl().

print_result(error, #{error := _}) ->
    ok;
print_result(Result, #{star := Star} = Meta) ->
    {Response, Expected} = format_expected(Result, Meta),
    M = format_module(Meta),
    Parameters = format_parameters(Meta),
    Time = format_time(Meta),
    Res = vt100format([bright, cyan], "~p", [Result]),

    %% Using vt100 codes \e[s (Save Cursor) to save the start position of the result.
    %% This is used by the expected string to place the expected result directley below if needed.
    io:format("~s~s~p~s -> \e[s~s \t~s~s~n", [Response, M, Star, Parameters, Res, Time, Expected]).

format_module(#{module := M}) ->
    io_lib:format("~p:", [M]);
format_module(_) ->
    "".

format_parameters(#{parameters := Parameters}) ->
    io_lib:format("(~0p)", [Parameters]);
format_parameters(_) ->
    "".

format_time(Time) when is_integer(Time), Time < 1000 ->
    vt100format(blue, "~7.3f ms", [Time / 1000]);
format_time(Time) when is_integer(Time), Time < 1000 * 1000 ->
    vt100format(green, "~7.3f ms", [Time / 1000]);
format_time(Time) when is_integer(Time), Time < 10 * 1000 * 1000 ->
    vt100format(yellow, "~7.3f s ", [Time / (1000 * 1000)]);
format_time(Time) when is_integer(Time) ->
    vt100format(red, "~7.3f s ", [Time / (1000 * 1000)]);
format_time(#{time := Time, read_time := ReadTime}) ->
    ["\e[60G", format_time(ReadTime), "  ", format_time(Time), "  ", format_time(ReadTime + Time)];
format_time(#{time := Time}) ->
    ["\e[84G", format_time(Time)];
format_time(_) ->
    "".

format_expected(Result, #{expected := Result}) ->
    {vt100format(green, "OK   ", []), ""};
format_expected(manual, #{expected := Expected}) ->
    %% Using vt100 codes \e[u (Restore Cursor) \e[B (Cursor Down)
    %% to place expected value in same column as the result.
    {vt100format(yellow, "MAN  ", []), io_lib:format("~nexpected: \e[u\e[B~p", [Expected])};
format_expected(_Result, #{expected := unknown}) ->
    {"", ""};
format_expected(_Result, #{expected := Other}) ->
    %% Using vt100 codes \e[u (Restore Cursor) \e[B (Cursor Down)
    %% to place expected value in same column as the result.
    {vt100format(red, "FAIL ", []), io_lib:format("~nexpected: \e[u\e[B~p", [Other])};
format_expected(_, _) ->
    {"", ""}.

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

check_answers(false, List) ->
    List;
check_answers(true, List) ->
    check_answers(List, []);
check_answers([], Acc) ->
    lists:reverse(Acc);
check_answers([{Res, #{type := input, problem := {Year, Day}, star := Star} = Meta} | Rest], Acc) ->
    Answer = aoc_web:get_answer(Year, Day, Star),
    check_answers(Rest, [{Res, Meta#{expected => Answer}} | Acc]);
check_answers([H | Rest], Acc) ->
    check_answers(Rest, [H | Acc]).

attirs() ->
    #{
        reset => "0",
        bright => "1",
        dim => "2",
        underscore => "4",
        blink => "5",
        reverse => "7",
        hidden => "8",

        black => "30",
        red => "31",
        green => "32",
        yellow => "33",
        blue => "34",
        magenta => "35",
        cyan => "36",
        white => "37",

        bg_black => "40",
        bg_red => "41",
        bg_green => "42",
        bg_yellow => "43",
        bg_blue => "44",
        bg_magenta => "45",
        bg_cyan => "46",
        bg_white => "47"
    }.

vt100code(reset) ->
    "\e[0m";
vt100code(Attirs) when is_list(Attirs) ->
    ["\e[0;", string:join([maps:get(A, attirs()) || A <- Attirs], ";"), "m"];
vt100code(Attir) ->
    ["\e[0;", maps:get(Attir, attirs()), "m"].

vt100format(Attirs, Format, Params) ->
    [
        vt100code(Attirs),
        io_lib:format(Format, Params),
        vt100code(reset)
    ].
