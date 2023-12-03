-module(aoc2022_day16).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day16.txt"),
    io:format("S1: ~p ~nS2: ~p ~n", [S1, S2]),
    Res.

run(Star, File) ->
    erlang:erase(),
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

%% TODO: Optimize by reducing to a graph with only nonzero nodes, with varrying distance on edges.

read(File) ->
    tools:read_lines(File, fun parse_valves/1).

star1(Lines) ->
    {Pos, Zero} = lists:partition(fun non_zero_rate/1, Lines),

    PosN = [{P, S#{n => 1 bsl I}} || {I, {P, S}} <- lists:enumerate(Pos)],

    dfs(30, "AA", maps:from_list(PosN ++ Zero), 0).

star2(Lines) ->
    {Pos, Zero} = lists:partition(fun non_zero_rate/1, Lines),
    PosN = [{P, S#{n => 1 bsl I}} || {I, {P, S}} <- lists:enumerate(0, Pos)],
    ALL = lists:sum([N || {_, #{n := N}} <- PosN]),
    State = maps:from_list(PosN ++ Zero),
    Start = "AA",
    lists:max([
        dfs(26, Start, State, ALL - Mask) + dfs(26, Start, State, Mask)
     || Mask <- lists:seq(0, ALL)
    ]).

parse_valves(L) ->
    {F, S} =
        case string:split(L, ";") of
            [First, " tunnels lead to valves " ++ Second] ->
                {First, Second};
            [First, " tunnel leads to valve " ++ Second] ->
                {First, Second}
        end,

    [[Tunnel, Rate]] = tools:parse_format(F, "Valve ~s has flow rate=~d"),
    To = string:split(S, ", ", all),
    {Tunnel, #{
        n => 0,
        rate => Rate,
        to => [T || T <- To]
    }}.

dfs(0, _, _, _) ->
    0;
dfs(TimeLeft, Position, State, Open) ->
    case erlang:get({Position, TimeLeft, Open}) of
        undefined ->
            Value =
                case maps:get(Position, State) of
                    #{
                        rate := Rate,
                        to := To,
                        n := N
                    } when
                        Rate > 0, N band Open == 0
                    ->
                        lists:max([
                            Rate * (TimeLeft - 1) +
                                dfs(TimeLeft - 1, Position, State, Open + N)
                            | [dfs(TimeLeft - 1, T, State, Open) || T <- To]
                        ]);
                    #{to := To} ->
                        lists:max([dfs(TimeLeft - 1, T, State, Open) || T <- To])
                end,
            erlang:put({Position, TimeLeft, Open}, Value),
            Value;
        V ->
            V
    end.

non_zero_rate({_, #{rate := Rate}}) ->
    Rate /= 0.
