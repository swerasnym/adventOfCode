-module(day18).

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
    lists:sum([calculate(shunt(Tokens, [], []), []) || Tokens <- Data]).

star2(Data) ->
    lists:sum([calculate(shunt2(Tokens, [], []), []) || Tokens <- Data]).

read(File) ->
    {ok, Bin} = file:read_file(File),
    [begin
         Line1 = string:replace(Line, "(", "( ", all),
         Line2 = string:replace(Line1, ")", " )", all),
         Tokens = string:split(Line2, " ", all)
     end
     || Line
            <- string:split(
                   string:trim(binary_to_list(Bin)), "\n", all)].

shunt([], Out, []) ->
    lists:reverse(Out);
shunt([], Out, [Op | Rest]) ->
    shunt([], [Op | Out], Rest);
shunt(["(" | Rest], Out, Ops) ->
    shunt(Rest, Out, ["(" | Ops]);
shunt([")" | Rest], Out, ["(" | Ops]) ->
    shunt(Rest, Out, Ops);
shunt([")" | _] = Tokens, Out, [Op | Ops]) ->
    shunt(Tokens, [Op | Out], Ops);
shunt([Op | Rest], Out, ["(" | _] = Ops) when Op == "+"; Op == "*" ->
    shunt(Rest, Out, [Op | Ops]);
shunt([Op1 | Rest], Out, [Op2 | Ops]) when Op1 == "+"; Op1 == "*" ->
    shunt(Rest, [Op2 | Out], [Op1 | Ops]);
shunt([Op1 | Rest], Out, []) when Op1 == "+"; Op1 == "*" ->
    shunt(Rest, Out, [Op1]);
shunt([Number | Rest], Out, Ops) ->
    shunt(Rest, [list_to_integer(Number) | Out], Ops).

shunt2([], Out, []) ->
    lists:reverse(Out);
shunt2([], Out, [Op | Rest]) ->
    shunt2([], [Op | Out], Rest);
shunt2(["(" | Rest], Out, Ops) ->
    shunt2(Rest, Out, ["(" | Ops]);
shunt2([")" | Rest], Out, ["(" | Ops]) ->
    shunt2(Rest, Out, Ops);
shunt2([")" | _] = Tokens, Out, [Op | Ops]) ->
    shunt2(Tokens, [Op | Out], Ops);
shunt2([Op1 | Rest], Out, []) when Op1 == "+"; Op1 == "*" ->
    shunt2(Rest, Out, [Op1]);
shunt2([Op | Rest], Out, ["(" | _] = Ops) when Op == "+"; Op == "*" ->
    shunt2(Rest, Out, [Op | Ops]);
shunt2(["+" | Rest], Out, ["+" | _] = Ops) ->
    shunt2(Rest, ["+" | Out], Ops);
shunt2(["+" | Rest], Out, ["*" | _] = Ops) ->
    shunt2(Rest, Out, ["+" | Ops]);
shunt2(["*" | _] = Rest, Out, ["+" | Ops]) ->
    shunt2(Rest, ["+" | Out], Ops);
shunt2(["*" | Rest], Out, ["*" | _] = Ops) ->
    shunt2(Rest, ["*" | Out], Ops);
shunt2([Number | Rest] = Tokens, Out, Ops) ->
    shunt2(Rest, [list_to_integer(Number) | Out], Ops).

calculate([], [Result]) ->
    Result;
calculate(["+" | Rest], [N1, N2 | Acc]) ->
    calculate(Rest, [N1 + N2 | Acc]);
calculate(["*" | Rest], [N1, N2 | Acc]) ->
    calculate(Rest, [N1 * N2 | Acc]);
calculate([N | Rest], Acc) ->
    calculate(Rest, [N | Acc]).
