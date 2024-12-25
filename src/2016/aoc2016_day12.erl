-module(aoc2016_day12).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day12_ex.txt", star1, 42},
        {"examples/2016/day12_ex.txt", star2, 42}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 12},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Commands) ->
    End = run_program(0, Commands, #{}),
    get_value(a, End).

star2(Commands) ->
    End = run_program(0, Commands, #{c => 1}),
    get_value(a, End).

read(File) ->
    Lines = tools:read_lines(File, fun(L) -> string:split(L, " ", all) end),
    Commands = [parse_command(L) || L <- Lines],
    maps:from_list(lists:enumerate(0, Commands)).

parse_command(["cpy", A, B]) ->
    {cpy, parse_value(A), parse_value(B)};
parse_command(["jnz", A, B]) ->
    {jnz, parse_value(A), parse_value(B)};
parse_command(["inc", A]) ->
    {inc, parse_value(A)};
parse_command(["dec", A]) ->
    {dec, parse_value(A)}.

parse_value("a") ->
    a;
parse_value("b") ->
    b;
parse_value("c") ->
    c;
parse_value("d") ->
    d;
parse_value(Integer) ->
    erlang:list_to_integer(Integer).

get_value(R, Map) when is_atom(R) ->
    maps:get(R, Map, 0);
get_value(V, _) when is_integer(V) ->
    V.

run_command({cpy, X, Y}, Map) ->
    {1, Map#{Y => get_value(X, Map)}};
run_command({inc, X}, Map) ->
    {1, Map#{X => get_value(X, Map) + 1}};
run_command({dec, X}, Map) ->
    {1, Map#{X => get_value(X, Map) - 1}};
run_command({jnz, X, Y}, Map) ->
    case get_value(X, Map) of
        0 ->
            {1, Map};
        _ ->
            {get_value(Y, Map), Map}
    end.

run_program(Sp, Commands, Memory) ->
    case maps:get(Sp, Commands, halt) of
        halt ->
            Memory;
        Command ->
            {Ds, M1} = run_command(Command, Memory),
            run_program(Sp + Ds, Commands, M1)
    end.
