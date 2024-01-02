-module(aoc2018_day16).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2018/day16_ex.txt", star1, 1}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 16},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Tests, _}) ->
    length([T || T <- Tests, length(test1(T)) >= 3]).

star2({Tests, Program}) ->
    OptMap = find_opt_codes(Tests),
    io:format("~p~n", [OptMap]),
    P = [[maps:get(Opt, OptMap), A, B, C] || [Opt, A, B, C] <- Program],
    S = aoc_watch_code:new(),
    End = aoc_watch_code:run(aoc_watch_code:load_program(S, P)),
    aoc_watch_code:get_reg(End, 0).

read(File) ->
    [Program, [] | Tests] = lists:reverse(tools:read_blocks(File)),
    {
        [parse_test(tools:parse_lines(L)) || L <- Tests],
        tools:parse_lines(Program, fun tools:parse_integers/1)
    }.

parse_test([Before, Instruction, After]) ->
    {
        tools:parse_format(Before, "Before: [~d, ~d, ~d, ~d]"),
        tools:parse_integers(Instruction),
        tools:parse_format(After, "After:  [~d, ~d, ~d, ~d]")
    }.
test1({B, [_Opt | Test], A}) ->
    [I || I := Fun <- aoc_watch_code:instructions_map(), test(Fun, {B, Test, A})].
test({B, [Opt | Test], A}) ->
    {Opt, [I || I := Fun <- aoc_watch_code:instructions_map(), test(Fun, {B, Test, A})]}.

test(I, {Before, [A, B, C], After}) ->
    S = aoc_watch_code:new(Before),
    I(S, A, B, C),
    After == aoc_watch_code:get_registers(S).

find_opt_codes(Tests) ->
    AllValid = maps:groups_from_list(fun({K, _}) -> K end, fun({_, V}) -> V end, [
        test(T)
     || T <- Tests
    ]),
    Overlaps = [{Opt, tools:overlap(Vs)} || Opt := Vs <- AllValid],
    filter_overlaps(lists:sort([{length(Vs), Opt, Vs} || {Opt, Vs} <- Overlaps]), []).

filter_overlaps([], Acc) ->
    maps:from_list(Acc);
filter_overlaps([{1, Opt, [Fun]} | Overlaps], Acc) ->
    NewOverlaps = lists:map(fun(In) -> remove_found(Fun, In) end, Overlaps),
    filter_overlaps(lists:sort(NewOverlaps), [{Opt, Fun} | Acc]).

remove_found(Fun, {N, Opt, Functions} = In) ->
    case lists:member(Fun, Functions) of
        true ->
            {N - 1, Opt, lists:delete(Fun, Functions)};
        false ->
            In
    end.
