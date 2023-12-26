-module(aoc2020_day18).
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
        problem => {2020, 18},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    lists:sum([calculate(shunt(Tokens, [], []), []) || Tokens <- Data]).

star2(Data) ->
    lists:sum([calculate(shunt2(Tokens, [], []), []) || Tokens <- Data]).

read(File) ->
    [
        begin
            Line1 = string:replace(Line, "(", "( ", all),
            Line2 = string:replace(Line1, ")", " )", all),
            string:split(Line2, " ", all)
        end
     || Line <- tools:read_lines(File)
    ].

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
shunt2([Number | Rest], Out, Ops) ->
    shunt2(Rest, [list_to_integer(Number) | Out], Ops).

calculate([], [Result]) ->
    Result;
calculate(["+" | Rest], [N1, N2 | Acc]) ->
    calculate(Rest, [N1 + N2 | Acc]);
calculate(["*" | Rest], [N1, N2 | Acc]) ->
    calculate(Rest, [N1 * N2 | Acc]);
calculate([N | Rest], Acc) ->
    calculate(Rest, [N | Acc]).
