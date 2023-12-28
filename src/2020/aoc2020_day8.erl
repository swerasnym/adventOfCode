-module(aoc2020_day8).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2020/data/dayN_ex.txt", star1, unknown},
        % {"2020/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2020, 8},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Instructions) ->
    {_Ic, Result} = run(Instructions),
    Result.

star2(Instructions) ->
    Results =
        [
            run(Instructions#{Ic => {update(Code), Value}})
         || {Ic, {Code, Value}} <- maps:to_list(Instructions), Code /= acc
        ],
    proplists:get_value(maps:size(Instructions) + 1, Results).

read(File) ->
    Ops = [{Op, Int} || [Op, Int] <- tools:read_multiple_formats(File, "~3a ~d")],
    maps:from_list(lists:zip(lists:seq(1, length(Ops)), Ops)).

run(Instructions) ->
    run(1, Instructions, 0).

run(Ic, Instructions, Acc) ->
    case maps:get(Ic, Instructions, loop) of
        loop ->
            {Ic, Acc};
        {nop, _} ->
            run(Ic + 1, maps:without([Ic], Instructions), Acc);
        {acc, Value} ->
            run(Ic + 1, maps:without([Ic], Instructions), Acc + Value);
        {jmp, Value} ->
            run(Ic + Value, maps:without([Ic], Instructions), Acc)
    end.

update(jmp) ->
    nop;
update(nop) ->
    jmp.
