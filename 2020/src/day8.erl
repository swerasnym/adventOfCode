-module(day8).

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

star1(Data) ->
    {_Ic, Result} = run(1, maps:from_list(Data), 0),
    Result.

star2(Data) ->
    Instructions = maps:from_list(Data),
    Results =
        [run(1, Instructions#{Ic => {update(Code), Value}}, 0)
         || {Ic, {Code, Value}} <- Data, Code /= acc],
    proplists:get_value(length(Data) + 1, Results).

read(File) ->
    {ok, Bin} = file:read_file(File),
    Ops = [parse_op(Op)
           || Op
                  <- string:split(
                         string:trim(binary_to_list(Bin)), "\n", all)],

    lists:zip(
        lists:seq(1, length(Ops)), Ops).

parse_op("nop " ++ Rest) ->
    {Int, []} = string:to_integer(Rest),
    {nop, Int};
parse_op("acc " ++ Rest) ->
    {Int, []} = string:to_integer(Rest),
    {acc, Int};
parse_op("jmp " ++ Rest) ->
    {Int, []} = string:to_integer(Rest),
    {jmp, Int}.

run(Ic, Instructions, Acc) ->
    case maps:get(Ic, Instructions, Acc) of
        Acc ->
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
