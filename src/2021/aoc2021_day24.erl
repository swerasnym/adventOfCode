-module(aoc2021_day24).
-behaviour(aoc_solution).

-export([run/0, run/2]).
-export([alu_inp/2, alu_add/2, alu_mul/2, alu_div/2, alu_mod/2, alu_eql/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 24},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Program, other}) ->
    alu_run(Program, []);
star1({Program, AocData}) ->
    Input = find_serial(AocData, max),
    {_, 0, 0, 0, []} = alu_run(Program, Input),
    combine(Input, 0).

star2({Program, other}) ->
    alu_run(Program, []);
star2({Program, AocData}) ->
    Input = find_serial(AocData, min),
    {_, 0, 0, 0, []} = alu_run(Program, Input),
    combine(Input, 0).

read(File) ->
    Lines = tools:read_lines(File, fun(L) -> string:split(L, " ", all) end),
    Program = [{list_to_atom("alu_" ++ I), Rest} || [I | Rest] <- Lines],

    AocFormat =
        "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z ~d\nadd x ~d\n"
        "eql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\n"
        "mul z y\nmul y 0\nadd y w\nadd y ~d\nmul y x\nadd z y",

    AocData =
        try
            tools:read_multiple_formats(File, AocFormat)
        catch
            error:_ ->
                other
        end,
    {Program, AocData}.

combine([], Acc) ->
    Acc;
combine([Hd | Rest], Acc) ->
    combine(Rest, Acc * 10 + Hd).

find_serial(AocData, MinMax) ->
    F = fun
        ([1, _, V2]) ->
            {push, V2};
        ([26, V1, _]) ->
            {pop, V1}
    end,
    find_serial([F(Row) || Row <- AocData], MinMax, 0, [], []).

find_serial([], _MinMax, _Pos, [], Result) ->
    {_, Input} = lists:unzip(lists:sort(Result)),
    Input;
find_serial([{push, V} | Rest], MinMax, Pos, Stack, Result) ->
    find_serial(Rest, MinMax, Pos + 1, [{Pos, V} | Stack], Result);
find_serial([{pop, V} | Rest], max, Pos, [{SPos, Sv} | Stack], Result) ->
    Diff = Sv + V,
    New =
        case Diff >= 0 of
            true ->
                [{Pos, 9}, {SPos, 9 - Diff}];
            false ->
                [{Pos, 9 + Diff}, {SPos, 9}]
        end,
    find_serial(Rest, max, Pos + 1, Stack, New ++ Result);
find_serial([{pop, V} | Rest], min, Pos, [{SPos, Sv} | Stack], Result) ->
    Diff = Sv + V,
    New =
        case Diff >= 0 of
            true ->
                [{Pos, 1 + Diff}, {SPos, 1}];
            false ->
                [{Pos, 1}, {SPos, 1 - Diff}]
        end,
    find_serial(Rest, min, Pos + 1, Stack, New ++ Result).

%% ALU implementation
alu_run(Program, Input) ->
    Call = fun({Inst, Par}, Reg) -> ?MODULE:Inst(Par, Reg) end,
    lists:foldl(Call, {0, 0, 0, 0, Input}, Program).

alu_get([P], Reg) when P >= $w, P =< $z ->
    element(P - $w + 1, Reg);
alu_get(Int, _Reg) ->
    list_to_integer(Int).

alu_set(5, Reg, Value) ->
    erlang:setelement(5, Reg, Value);
alu_set([P], Reg, Value) when P >= $w, P =< $z ->
    erlang:setelement(P - $w + 1, Reg, Value).

alu_inp([A] = Par, Reg) ->
    {X, Value} =
        case element(5, Reg) of
            [] ->
                {ok, [Read]} = io:fread("Input into " ++ Par ++ "> ", "~d"),
                {Read, []};
            [Hd | V] ->
                {Hd, V}
        end,
    Reg1 = alu_set(5, Reg, Value),
    alu_set(A, Reg1, X).

alu_add([A, B], Reg) ->
    X1 = alu_get(A, Reg),
    X2 = alu_get(B, Reg),
    alu_set(A, Reg, X1 + X2).

alu_mul([A, B], Reg) ->
    X1 = alu_get(A, Reg),
    X2 = alu_get(B, Reg),
    alu_set(A, Reg, X1 * X2).

alu_div([A, B], Reg) ->
    X1 = alu_get(A, Reg),
    X2 = alu_get(B, Reg),
    alu_set(A, Reg, X1 div X2).

alu_mod([A, B], Reg) ->
    X1 = alu_get(A, Reg),
    X2 = alu_get(B, Reg),
    alu_set(A, Reg, X1 rem X2).

alu_eql([A, B], Reg) ->
    X1 = alu_get(A, Reg),
    X2 = alu_get(B, Reg),
    case X1 == X2 of
        true ->
            alu_set(A, Reg, 1);
        false ->
            alu_set(A, Reg, 0)
    end.
