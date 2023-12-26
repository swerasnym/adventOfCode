-module(aoc2019_day22).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 22},
        examples => Examples,
        all => [star1, {star1, old}, star2]
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data, old) ->
    Shuffled = lists:foldl(fun instruction/2, lists:seq(0, 10006), Data),

    proplists:get_value(2019, lists:zip(Shuffled, lists:seq(0, 10006))).

star1(Data) ->
    Cards = 10007,
    F = lists:foldl(
        fun(Command, Function) -> instruction3(Command, Function, Cards) end,
        {1, x, 0},
        Data
    ),
    eval(F, 2019, Cards).

star2(Data) ->
    Cards = 119315717514047,
    Shuffles = 101741582076661,
    F = lists:foldl(
        fun(Command, Function) -> instruction3(Command, Function, Cards) end,
        {1, x, 0},
        Data
    ),

    {A, x, B} = pow_fun(F, Shuffles, Cards),

    Ainv = mod_inv(A, Cards),
    posrem(Ainv * (2020 - B), Cards).

read(File) ->
    {ok, Bin} = file:read_file(File),
    List = binary_to_list(Bin),
    string:split(string:trim(List, trailing, "\n"), "\n", all).

instruction("deal into new stack", Deck) ->
    lists:reverse(Deck);
instruction("cut " ++ NumberStr, Deck) ->
    Number = list_to_integer(NumberStr),
    {Top, Bottom} =
        case Number >= 0 of
            true ->
                lists:split(Number, Deck);
            false ->
                lists:split(length(Deck) + Number, Deck)
        end,
    Bottom ++ Top;
instruction("deal with increment " ++ NumberStr, Deck) ->
    Size = length(Deck),
    Number = list_to_integer(NumberStr),
    Order =
        [
            {Pos rem Size, Card}
         || {Pos, Card} <- lists:zip(lists:seq(0, Number * Size - 1, Number), Deck)
        ],
    [Card || {_Pos, Card} <- lists:sort(Order)].

% Creates a linear function mod Cards to do one shuffle
instruction3("deal into new stack", F, Cards) ->
    join({-1, x, -1}, F, Cards);
instruction3("cut " ++ NumberStr, F, Cards) ->
    C = list_to_integer(NumberStr),
    join({1, x, -C}, F, Cards);
instruction3("deal with increment " ++ NumberStr, F, Cards) ->
    C = list_to_integer(NumberStr),
    join({C, x, 0}, F, Cards).

posrem(A, Mod) ->
    (A rem Mod + Mod) rem Mod.

%% Calculate N times itarated function of F to some modulus
pow_fun(_F, 0, _Mod) ->
    {1, x, 0};
pow_fun(F, 1, _Mod) ->
    F;
pow_fun(F, N, Mod) when N rem 2 == 0 ->
    Half = pow_fun(F, N div 2, Mod),
    join(Half, Half, Mod);
pow_fun(F, N, Mod) when N rem 2 == 1 ->
    Rest = pow_fun(F, N - 1, Mod),
    join(F, Rest, Mod).

join({A, x, B}, {C, x, D}, Mod) ->
    {posrem(A * C, Mod), x, posrem(A * D + B, Mod)}.

eval({A, x, B}, Value, Mod) ->
    posrem(A * Value + B, Mod).

mod_inv(A, M) ->
    case extended_gcd(A, M) of
        {1, X, _Y} ->
            posrem(X, M);
        _ ->
            none
    end.

extended_gcd(0, B) ->
    {B, 0, 1};
extended_gcd(A, B) ->
    {G, X, Y} = extended_gcd(B rem A, A),
    {G, Y - B div A * X, X}.
