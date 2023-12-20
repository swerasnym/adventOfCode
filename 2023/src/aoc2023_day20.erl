-module(aoc2023_day20).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"2023/data/day20_ex.txt", star1, 32000000},
        {"2023/data/day20_ex2.txt", star1, 11687500}
        %{"2023/data/day20_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2023, 20},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Map) ->
    Memmory = initial_state(Map),
    io:format("~p~n~n", [Memmory]),

    Fun = fun({Steps, Mem}) -> act_loop([{broadcaster, low, button}], Map, Mem, Steps) end,

    {{H, L}, _} = tools:repeat(1000, Fun, {{0, 0}, Memmory}),
    H * L.

%% Originaly solved semi manually by logging (high) inputs to the one module
%% sending messages to rx, and guessing this happens in separate cycles.
%% This can be verified by plotitng the input via graphwiz.
star2(Map) ->
    Memmory = initial_state(Map),
    [Speciall] = [Sp || {"rx", Sp} <- inputs(Map)],
    act_loop2([], Map, Memmory, 0, {Speciall, #{}}).

read(File) ->
    maps:from_list(tools:read_lines(File, fun parse_module/1)).

parse_module("broadcaster -> " ++ Rest) ->
    {broadcaster, {broadcast, string:tokens(Rest, " ,")}};
parse_module("%" ++ Rest) ->
    [Module, Rest2] = string:split(Rest, " -> "),
    {Module, {flip_flop, string:tokens(Rest2, " ,")}};
parse_module("&" ++ Rest) ->
    [Module, Rest2] = string:split(Rest, " -> "),
    {Module, {conjunction, string:tokens(Rest2, " ,")}}.

act_loop([], _Map, Memmory, Steps) ->
    {Steps, Memmory};
act_loop([{_To, Signal, _From} = Head | Rest], Map, Memmory, Steps) ->
    %  io:format("~p -~p-> ~p~n", [From, Signal, To]),
    {Memmory1, Next} = act(Head, Map, Memmory),
    act_loop(Rest ++ Next, Map, Memmory1, add(Signal, Steps)).

act({M, Signal, From}, Map, Memmory) ->
    case maps:get(M, Map, nothing) of
        nothing ->
            {Memmory, []};
        {broadcast, Modules} ->
            {Memmory, [{To, low, M} || To <- Modules]};
        {flip_flop, Modules} when Signal == low ->
            {NewState, NewSignal} = flip(maps:get(M, Memmory)),
            {Memmory#{M := NewState}, [{To, NewSignal, M} || To <- Modules]};
        {flip_flop, _Modules} when Signal == high ->
            {Memmory, []};
        {conjunction, Modules} ->
            CM0 = maps:get(M, Memmory),
            CM1 = CM0#{From := Signal},
            case lists:all(fun(L) -> L == high end, maps:values(CM1)) of
                true ->
                    {Memmory#{M := CM1}, [{To, low, M} || To <- Modules]};
                false ->
                    {Memmory#{M := CM1}, [{To, high, M} || To <- Modules]}
            end
    end.

act_loop2([], Map, Memmory, Presses, Sp) ->
    act_loop2([{broadcaster, low, button}], Map, Memmory, Presses + 1, Sp);
act_loop2([{Sp, high, From} = Head | Rest], Map, Memmory, Presses, {Sp, SpMap}) ->
    % io:format("~p: ~p -~p-> ~p~n", [Presses, From, high, Sp]),
    SpMap1 =
        case maps:get(From, SpMap, first) of
            first ->
                SpMap#{From => Presses};
            {loop, _} ->
                SpMap;
            Prev ->
                SpMap#{From := {loop, Presses - Prev}}
        end,

    Pred = fun
        ({loop, _}) -> true;
        (_) -> false
    end,
    case lists:all(Pred, maps:values(SpMap1)) of
        true ->
            io:format("~p~n", [SpMap1]),
            tools:lcm([V || {loop, V} <- maps:values(SpMap1)]);
        false ->
            {Memmory1, Next} = act(Head, Map, Memmory),
            act_loop2(Rest ++ Next, Map, Memmory1, Presses, {Sp, SpMap1})
    end;
act_loop2([Head | Rest], Map, Memmory, Presses, Sp) ->
    {Memmory1, Next} = act(Head, Map, Memmory),
    act_loop2(Rest ++ Next, Map, Memmory1, Presses, Sp).

flip(on) -> {off, low};
flip(off) -> {on, high}.

inputs(Map) ->
    [{M, I} || I := {_, Outs} <- Map, M <- Outs].

initial_state(Map) ->
    Inputs = inputs(Map),
    maps:from_list([{Mod, init(Type, Mod, Inputs)} || Mod := {Type, _} <- Map]).

init(broadcast, _M, _) ->
    nothing;
init(flip_flop, _M, _) ->
    off;
init(conjunction, M, Inputs) ->
    #{I => low || {Mo, I} <- Inputs, Mo == M}.

add(low, {L, H}) -> {L + 1, H};
add(high, {L, H}) -> {L, H + 1}.
