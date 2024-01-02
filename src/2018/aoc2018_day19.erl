-module(aoc2018_day19).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2018/day19_ex.txt", star1, 7}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2018, 19},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1({IpReg, Program}) ->
    S0 = aoc_watch_code:new(),
    S1 = aoc_watch_code:set_ip_reg(S0, IpReg),
    End = aoc_watch_code:run(aoc_watch_code:load_program(S1, Program)),
    aoc_watch_code:get_reg(End, 0).

star2({IpReg, Program}) ->
    %% Peek at instructions 13 to get our special register!
    [eqrr, _, RN, _] = lists:nth(4 + 1, Program),

    S0 = aoc_watch_code:new(),
    S1 = aoc_watch_code:set_ip_reg(S0, IpReg),
    S2 = aoc_watch_code:set_reg(S1, 0, 1),

    %% Run 23 instructions
    End = aoc_watch_code:run_dbg(aoc_watch_code:load_program(S2, Program), 23),

    %% Assume that we are given a program that calculates the sum of divisors of
    %% the value in register 4 at this point.
    N = aoc_watch_code:get_reg(End, RN),
    Divisors = divisors(N),
    io:format("~p~n", [Divisors]),

    lists:sum(divisors(N)).

read(File) ->
    [IpLine | Program] = tools:read_lines(File),
    [Ip] = tools:parse_format(IpLine, "#ip ~d"),
    {Ip, [tools:parse_format(Line, "~a ~d ~d ~d") || Line <- Program]}.

divisors(N) ->
    lists:sort(divisors(1, N, [])).
divisors(D, N, Acc) when D * D > N ->
    Acc;
divisors(D, N, Acc) ->
    case N rem D of
        0 ->
            divisors(D + 1, N, [D, N div D | Acc]);
        _ ->
            divisors(D + 1, N, Acc)
    end.
