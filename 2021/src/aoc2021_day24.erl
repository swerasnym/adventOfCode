-module(aoc2021_day24).

-export([run/2, profile/3, eprof/2]).
-export([alu_inp/2, alu_add/2, alu_mul/2, alu_div/2, alu_mod/2, alu_eql/2]).

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

profile(Star, File, Times) ->
    Data = read(File),
    case Star of
        star1 ->
            profile(fun() -> star1(Data) end, Times);
        star2 ->
            profile(fun() -> star2(Data) end, Times);
        _ ->
            Star1 = profile(fun() -> star1(Data) end, Times),
            Star2 = profile(fun() -> star2(Data) end, Times),
            {Star1, Star2}
    end.

profile(F, Times) ->
    Expected = F(),
    Results =
        [begin
             {Time, Expected} = timer:tc(F),
             Time
         end
         || _ <- lists:seq(1, Times)],
    {Expected, lists:sum(Results) / Times / 1000}.

eprof(Star, File) ->
    eprof:start(),
    eprof:start_profiling([self()]),
    Result = run(Star, File),
    eprof:stop_profiling(),
    eprof:analyze(),
    eprof:stop(),
    Result.

star1({Program, other}) ->
    alu_run(Program, []);
star1({Program, AocData}) ->
    Input = find_serial(AocData, max),
    {_, 0, 0, 0, []} = alu_run(Program, Input),
    combind(Input, 0).

star2({Program, other}) ->
    alu_run(Program, []);
star2({Program, AocData}) ->
    Input = find_serial(AocData, min),
    {_, 0, 0, 0, []} = alu_run(Program, Input),
    combind(Input, 0).

read(File) ->
    Lines = tools:read_lines(File, fun(L) -> string:split(L, " ", all) end),
    Program = [{list_to_atom("alu_" ++ I), Rest} || [I | Rest] <- Lines],

    AocFormat =
        "inp w\nmul x 0\nadd x z\nmod x 26\ndiv z ~d\nadd x ~d\n"
        "eql x w\neql x 0\nmul y 0\nadd y 25\nmul y x\nadd y 1\n"
        "mul z y\nmul y 0\nadd y w\nadd y ~d\nmul y x\nadd z y",

    AocData =
        try
            tools:read_format(File, AocFormat)
        catch
            error:_ ->
                other
        end,
    {Program, AocData}.

combind([], Acc) ->
    Acc;
combind([Hd | Rest], Acc) ->
    combind(Rest, Acc * 10 + Hd).

find_serial(AocData, MinMax) ->
    F = fun ([1, _, V2]) ->
                {push, V2};
            ([26, V1, _]) ->
                {pop, V1}
        end,
    find_serial([F(Row) || Row <- AocData], MinMax, 0, [], []).

find_serial([], _MinMax, _Pos, [], Result) ->
    {_, Input} =
        lists:unzip(
            lists:sort(Result)),
    Input;
find_serial([{push, V} | Rest], MinMax, Pos, Stack, Result) ->
    find_serial(Rest, MinMax, Pos + 1, [{Pos, V} | Stack], Result);
find_serial([{pop, V} | Rest], max, Pos, [{SPos, Sv} | Stack], Result) ->
    Diff = Sv + V,
    New = if Diff >= 0 ->
                 [{Pos, 9}, {SPos, 9 - Diff}];
             Diff < 0 ->
                 [{Pos, 9 + Diff}, {SPos, 9}]
          end,
    find_serial(Rest, max, Pos + 1, Stack, New ++ Result);
find_serial([{pop, V} | Rest], min, Pos, [{SPos, Sv} | Stack], Result) ->
    Diff = Sv + V,
    New = if Diff >= 0 ->
                 [{Pos, 1 + Diff}, {SPos, 1}];
             Diff < 0 ->
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
    setelement(5, Reg, Value);
alu_set([P], Reg, Value) when P >= $w, P =< $z ->
    setelement(P - $w + 1, Reg, Value).

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
