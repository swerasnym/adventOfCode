-module(aoc2024_day24).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, star2/2, read/1]).

info() ->
    Examples = [
        {"examples/2024/day24_ex.txt", star1, 2024},
        {input, {star2, dumb}, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2024, 24},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({Init, Gates}) ->
    Res = calculate(Gates, [], Init),
    value(Res, $z).

star2({Init, Gates}) ->
    ok.

star2({Init, Gates0}, dumb) ->
    Swap = #{
        "z08" => "mvb",
        "mvb" => "z08",
        "jss" => "rds",
        "rds" => "jss",
        "z23" => "bmn",
        "bmn" => "z23",
        "z18" => "wss",
        "wss" => "z18"
    },

    Gates = [swap(Op, Swap) || Op <- Gates0],

    %io:format("~p", [lists:sort(Gates)]),
    Map1 = maps:from_list([L || L <- [relabel1(Op) || Op <- Gates], L /= skip]),
    Relabled1 = [relabel(Op, Map1) || Op <- Gates],
    %io:format("~p", [lists:sort(Relabled1)]),
    Map2 = maps:from_list([L || L <- [relabel2(Op) || Op <- Relabled1], L /= skip]),
    %io:format("~kp", [Map2]),
    Relabled2 = [relabel(Op, Map2) || Op <- Relabled1],

    %io:format("~p", [lists:sort(Relabled2)]),
    Map3 = maps:from_list([L || L <- [relabel3(Op) || Op <- Relabled2], L /= skip]),
    % io:format("~kp", [Map3]),
    Relabled3 = [relabel(Op, Map3) || Op <- Relabled2],
    io:format("~p", [lists:sort(Relabled3)]),
    Problem = [Op || Op <- Relabled3, check(Op)],
    io:format("~p~n", [Problem]),
    Trouble = [P || Tuple <- Problem, P <- tuple_to_list(Tuple), check2(P)],
    io:format("~p~n", [tools:count(Trouble)]),
    io:format("~s", [string:join(lists:usort(Trouble), ",")]),

    Res0 = calculate(Gates0, [], Init),
    Z0 = value(Res0, $z),
    X0 = value(Init, $x),
    Y0 = value(Init, $y),
    Diff0 = lists:reverse(integer_to_list(Z0 bxor (X0 + Y0), 2)),
    % io:format("~p~n", [lists:enumerate(0, [D - $0 || D <- Diff0])]),

    Res = calculate(Gates, [], Init),
    Z = value(Res, $z),
    X = value(Init, $x),
    Y = value(Init, $y),
    Diff = lists:reverse(integer_to_list(Z bxor (X + Y), 2)),

    io:format("~p~n", [lists:enumerate(0, [D - $0 || D <- Diff])]),

    io:format("Z:~p~n", [integer_to_list(Z, 2)]),
    io:format("s:~p~n", [integer_to_list(X + Y, 2)]),
    io:format("x:~p~n", [integer_to_list(Z bxor (X + Y), 2)]),
    io:format("X: ~p~n", [integer_to_list(X, 2)]),
    io:format("Y: ~p~n", [integer_to_list(Y, 2)]),
    string:join(lists:usort(maps:keys(Swap)), ",").
%length(Problem).

read(File) ->
    [Init, Gatesations] = tools:read_blocks(File),
    {
        maps:from_list(tools:parse_lines(Init, fun parse_init/1)),
        tools:parse_lines(Gatesations, fun parse_Gatesations/1)
    }.

parse_init(L) ->
    [Key, Value] = string:split(L, ": "),
    {Key, list_to_integer(Value)}.

parse_Gatesations(L) ->
    [Key1, Op, Key2, "->", Res] = string:split(L, " ", all),
    [K1, K2] = lists:sort([Key1, Key2]),
    {K1, Op, K2, Res}.

value(Map, Letter) ->
    Binary = [V + $0 || [L | _] := V <- maps:iterator(Map, ordered), L == Letter],
    list_to_integer(lists:reverse(Binary), 2).

calculate([], [], Map) ->
    Map;
calculate([], Skipped, Map) ->
    calculate(Skipped, [], Map);
calculate([{Key1, Op, Key2, Res} = K | Rest], Skipped, Map) ->
    case {maps:get(Key1, Map, missing), maps:get(Key2, Map, missing)} of
        {missing, _} ->
            calculate(Rest, [K | Skipped], Map);
        {_, missing} ->
            calculate(Rest, [K | Skipped], Map);
        {V1, V2} ->
            calculate(Rest, Skipped, Map#{Res => op(Op, V1, V2)})
    end.

op("XOR", V1, V2) ->
    V1 bxor V2;
op("AND", V1, V2) ->
    V1 band V2;
op("OR", V1, V2) ->
    V1 bor V2.

relabel1({"x00", "XOR", "y00", "z00"}) ->
    skip;
relabel1({"x" ++ D, "XOR", "y" ++ D, Res}) ->
    {Res, {"s", D, Res}};
relabel1({"x" ++ D, "AND", "y" ++ D, Res}) ->
    {Res, {"c", D, Res}};
relabel1(Op) ->
    skip.

swap({A, Op, B, Res}, Map) ->
    {A, Op, B, maps:get(Res, Map, Res)}.

relabel({A, Op, B, Res}, Map) ->
    [Ao, Bo] = lists:sort([maps:get(A, Map, A), maps:get(B, Map, B)]),
    {Ao, Op, Bo, maps:get(Res, Map, Res)}.

relabel2({"x00", "XOR", "y00", "z00"}) ->
    skip;
relabel2({{"s", D, _}, "XOR", Lab, "z" ++ D}) ->
    {Lab, {"cin", D, Lab}};
relabel2(Op) ->
    skip.

relabel3({"x00", "XOR", "y00", "z00"}) ->
    skip;
relabel3({{"cin", D, _}, "AND", {"s", D, _}, Res}) ->
    {Res, {"cmi", D, Res}};
relabel3(Op) ->
    skip.

check({"x00", "XOR", "y00", "z00"}) -> false;
check({_, "AND", _, "z" ++ _D}) -> true;
check({{_, D, _}, _Op, {_, D, _}, {_, D, _}}) -> false;
check({{_, D1, _}, _Op, {_, D1, _}, {_, D2, _}}) -> list_to_integer(D1) + 1 /= list_to_integer(D2);
check({"x" ++ D, _Op, "y" ++ D, {_, D, _}}) -> false;
check({{_, D, _}, "XOR", {_, D, _}, "z" ++ D}) -> false;
check({{_, D, _}, _Op, {_, D, _}, _}) -> false;
check({{_, D, _}, _Op, _, {_, D, _}}) -> false;
check({{_, D1, _}, _Op, {_, D2, _}, _}) -> list_to_integer(D1) + 1 /= list_to_integer(D2);
check({{_, D1, _}, _Op, _, {_, D2, _}}) -> list_to_integer(D1) + 1 /= list_to_integer(D2);
check({{_, D, _}, "XOR", _, "z" ++ D}) -> false;
check({{"c", "00", _}, _Op, _, _}) -> false;
check(_) -> true.

check2({_, _, _}) -> false;
% check2("z" ++ D) -> false;
check2("AND") -> false;
check2("OR") -> false;
check2("XOR") -> false;
check2(_) -> true.

% [{{"s","08","mcr"},"XOR","sjd","mvb"},
%  {{"s","18","fmm"},"XOR","mfk","wss"},
%  {{"c","14","rds"},"AND","scs","dcv"},
%  {{"c","23","fwj"},"OR","vsq","z23"},
%  {"shw","OR","wss",{"cin","19","nws"}},
%  {{"s","23","qmd"},"AND","bpr","vsq"},
%  {{"s","08","mcr"},"AND","sjd","z08"},
%  {{"s","18","fmm"},"AND","mfk","shw"}]
% #{"bpr" => 1,"dcv" => 1,"mfk" => 2,"mvb" => 1,"scs" => 1,"shw" => 2,
%   "sjd" => 2,"vsq" => 2,"wss" => 2}

% dcv,mfk,mvb,scs,shw,sjd,vsq,wss
% bpr,mfk,mvb,scs,shw,sjd,vsq,wss
% bpr,dcv,mfk,scs,shw,sjd,vsq,wss
% bpr,dcv,mfk,mvb,shw,sjd,vsq,wss
res() ->
    lists:usort(["wss", "vsq", "sjd", "shw", "mfk", "dcv", "bpr"]).
