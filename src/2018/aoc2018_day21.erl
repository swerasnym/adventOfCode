-module(aoc2018_day21).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 21},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({IpReg, Program}) ->
    erlang:erase(),
    %% Peek at instructions 28 to get the register to check...
    [eqrr, RE, 0, _] = lists:nth(28 + 1, Program),
    S0 = aoc_watch_code:new(),
    S1 = aoc_watch_code:set_ip_reg(S0, IpReg),
    End = aoc_watch_code:run_dbg2(aoc_watch_code:load_program(S1, Program), 1, {28, [RE]}),
    aoc_watch_code:get_reg(End, RE).

star2({IpReg, Program}) ->
    erlang:erase(),
    %% Peek at instructions 13 and 28
    [gtir, 256, RT, _] = lists:nth(13 + 1, Program),
    [eqrr, RE, 0, _] = lists:nth(28 + 1, Program),

    %% Patch in a divide by 256 instruction and a new jump on lines 17 and 18...
    {Head, [_, _ | Tail]} = lists:split(17, Program),
    Program1 = Head ++ [[divi, RT, 256, RT], [seti, 7, 0, IpReg]] ++ Tail,

    %% Load and run the program until a loop (or line 28 run 100 000 times)
    S0 = aoc_watch_code:new(),
    S1 = aoc_watch_code:set_ip_reg(S0, IpReg),
    aoc_watch_code:run_dbg2(
        aoc_watch_code:load_program(S1, Program1), 100_000, {28, [RE]}
    ),

    %% Find the last value before the repeat
    Values = erlang:get(),
    Ordered = tools:reverse_sort([{N, V} || {[V], N} <- Values]),
    hd(seen(Ordered, #{}, [])).

read(File) ->
    [IpLine | Program] = tools:read_lines(File),
    [Ip] = tools:parse_format(IpLine, "#ip ~d"),
    {Ip, [tools:parse_format(Line, "~a ~d ~d ~d") || Line <- Program]}.

seen([], _, Acc) ->
    Acc;
seen([{_, Reg} | Rest], Map, Acc) when is_map_key(Reg, Map) ->
    seen(Rest, Map, Acc);
seen([{_, Reg} | Rest], Map, Acc) ->
    seen(Rest, Map#{Reg => true}, [Reg | Acc]).
