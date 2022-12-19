-module(aoc2022_day19).

-export([run/0, run/2]).

run() ->
    {S1, S2} = Res = run(all, "../data/day19.txt"),
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

read(File) ->
    Read =
        tools:read_format(File,
                          "Blueprint ~d: Each ore robot costs ~d ore. Each clay robot "
                          "costs ~d ore. Each obsidian robot costs ~d ore and ~d clay. "
                          "Each geode robot costs ~d ore and ~d obsidian."),
    [{N,
      #{ore => [{OOre, ore}],
        clay => [{OClay, ore}],
        obsidian => [{OObsidian, ore}, {CObsidian, clay}],
        geode => [{OGeode, ore}, {ObGeode, obsidian}]}}
     || [N, OOre, OClay, OObsidian, CObsidian, OGeode, ObGeode] <- Read].

star1(Data) ->
    L = [begin
             G = bfs(24,
                     Blueprint,
                     [{#{ore => 0,
                         clay => 0,
                         obsidian => 0,
                         geode => 0},
                       #{ore => 1,
                         clay => 0,
                         obsidian => 0,
                         geode => 0}}]),

             {N, _} = Blueprint,
             io:format("Bp ~p Geodes ~p ~n", [N, G]),
             G * N
         end
         || Blueprint <- Data],

    S = lists:sum(L),
    io:format("~p ~w~n", [S, L]),
    S.

star2(Data) ->
    L = [begin
             G = bfs(32,
                     Blueprint,
                     [{#{ore => 0,
                         clay => 0,
                         obsidian => 0,
                         geode => 0},
                       #{ore => 1,
                         clay => 0,
                         obsidian => 0,
                         geode => 0}}]),
             {N, _} = Blueprint,
             io:format("Bp ~p Geodes  ~p ~n", [N, G]),
             G
         end
         || Blueprint <- lists:sublist(Data, 3)],
    S = tools:product(L),
    io:format("~p ~w~n", [S, L]),
    S.

heuristic(Time,
          {_Res =
               #{ore := Ore,
                 clay := Clay,
                 obsidian := Obsidian,
                 geode := Geode},
           _Rob =
               #{ore := ROre,
                 clay := RClay,
                 obsidian := RObsidian,
                 geode := RGeode}}) ->
    Ore
    + 10 * Clay
    + 100 * Obsidian
    + 10000 * Geode
    + Time * (ROre + 10 * RClay + 100 * RObsidian + 10000 * RGeode).

buy(Type,
    Costs,
    Resourses =
        #{ore := Ore,
          clay := Clay,
          obsidian := Obsidian},
    Robots) ->
    case {Type, maps:get(Type, Costs)} of
        {ore, [{OOre, ore}]} when OOre =< Ore ->
            {Resourses#{ore => Ore - OOre}, Robots#{Type => maps:get(Type, Robots) + 1}};
        {clay, [{OClay, ore}]} when OClay =< Ore ->
            {Resourses#{ore => Ore - OClay}, Robots#{Type => maps:get(Type, Robots) + 1}};
        {obsidian, [{OObsidian, ore}, {CObsidian, clay}]}
            when OObsidian =< Ore, CObsidian =< Clay ->
            {Resourses#{ore => Ore - OObsidian, clay => Clay - CObsidian},
             Robots#{Type => maps:get(Type, Robots) + 1}};
        {geode, [{OGeode, ore}, {ObGeode, obsidian}]} when OGeode =< Ore, ObGeode =< Obsidian ->
            {Resourses#{ore => Ore - OGeode, obsidian => Obsidian - ObGeode},
             Robots#{Type => maps:get(Type, Robots) + 1}};
        _ ->
            {Resourses, Robots}
    end.

collect(#{ore := Ore,
          clay := Clay,
          obsidian := Obsidian,
          geode := G},
        #{ore := ROre,
          clay := RClay,
          obsidian := RObsidian,
          geode := RG}) ->
    #{ore => Ore + ROre,
      clay => Clay + RClay,
      obsidian => Obsidian + RObsidian,
      geode => G + RG}.

bfs(0, {_, _Costs}, L) ->
    lists:max([maps:get(geode, Resourses) || {Resourses, _} <- L]);
bfs(Time, Bp = {_, Costs}, L) ->
    H = lists:usort([{heuristic(Time + 10, State), State} || State <- L]),
    New = [begin
               Buy = [buy(Type, Costs, ResoursesIn, Robots)
                      || Type <- [ore, clay, obsidian, geode]],
               [{collect(ResOut, Robots), RobOut} || {ResOut, RobOut} <- Buy]
           end
           || {_, {ResoursesIn, Robots}} <- lists:sublist(lists:reverse(H), 1500)],

    bfs(Time - 1, Bp, lists:flatten(New)).
