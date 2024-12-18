-module(aoc2016_day10).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star1/2, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day10_ex.txt", {star1, [2, 5]}, 2},
        {"examples/2016/day10_ex.txt", star2, 30}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 10},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    star1(Data, [17, 61]).

star1({Values, Bots}, Chips) ->
    BotActions = #{B => {H, L} || {B, H, L} <- Bots},
    {_, Comp} = simulate(Values, BotActions, {#{}, #{}}),
    maps:get(Chips, Comp).

star2({Values, Bots}) ->
    BotActions = #{B => {H, L} || {B, H, L} <- Bots},
    {State, _} = simulate(Values, BotActions, {#{}, #{}}),
    [V0] = maps:get({output, 0}, State),
    [V1] = maps:get({output, 1}, State),
    [V2] = maps:get({output, 2}, State),
    V0 * V1 * V2.

read(File) ->
    Parsed = tools:read_lines(File, fun parse/1),
    lists:partition(fun({T, _, _}) -> T == value end, Parsed).

parse("value " ++ Rest) ->
    [V, B] = tools:parse_format(Rest, "~d goes to bot ~d"),
    {value, V, {bot, B}};
parse("bot " ++ Rest) ->
    [B, T1, N1, T2, N2] = tools:parse_format(Rest, "~d gives low to ~a ~d and high to ~a ~d"),
    {{bot, B}, {T1, N1}, {T2, N2}}.

simulate([], _, {State, Comp}) ->
    {State, Comp};
simulate([I | Rest], BotActions, SC) ->
    simulate(Rest, BotActions, step(I, BotActions, SC)).

step({value, V, Out = {output, _}}, _BotActions, {State, Comp}) ->
    Current = maps:get(Out, State, []),
    {State#{Out => [V | Current]}, Comp};
step({value, V, Bot = {bot, B}}, BotActions, {State, Comp}) ->
    case maps:get(Bot, State, []) of
        [] ->
            {State#{Bot => [V]}, Comp};
        [Mem] ->
            C = [L, H] = lists:sort([V, Mem]),
            {TL, TH} = maps:get(Bot, BotActions),
            State0 = State#{Bot => []},
            Comp0 = Comp#{C => B},
            SC1 = step({value, L, TL}, BotActions, {State0, Comp0}),
            step({value, H, TH}, BotActions, SC1)
    end.
