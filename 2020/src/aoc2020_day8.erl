-module(aoc2020_day8).

-export([run/2]).

run(Star, File) ->
    Data = read(File),

    case Star of
        star1 ->
            star1(Data);
        star2 ->
            star2(Data);
        _ ->
            Star1 = star1(Data),
            Star2 = star2(Data),
            {Star1, Star2}
    end.

star1(Instructions) ->
    {_Ic, Result} = run(Instructions),
    Result.

star2(Instructions) ->
    Results =
        [run(Instructions#{Ic => {update(Code), Value}})
         || {Ic, {Code, Value}} <- maps:to_list(Instructions), Code /= acc],
    proplists:get_value(maps:size(Instructions) + 1, Results).

read(File) ->
    Ops = [{Op, Int} || [Op, Int] <- tools:read_format(File, "~3a ~d")],
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
