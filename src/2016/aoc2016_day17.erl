-module(aoc2016_day17).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {{data, "ihgpwlah"}, star1, "DDRRRD"},
        {{data, "ihgpwlah"}, star2, 370},
        {{data, "kglvqrro"}, star1, "DDUDRLRRUDRD"},
        {{data, "kglvqrro"}, star2, 492},
        {{data, "ulqzkmiv"}, star1, "DRURDRUDDLLDLUURRDULRLDUUDDDRR"},
        {{data, "ulqzkmiv"}, star2, 830}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 17},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Passcode) ->
    bfs([{0, {0, 0}, Passcode}]) -- Passcode.

star2(Passcode) ->
    dfs({0, {0, 0}, Passcode}).

read(File) ->
    tools:read_string(File).

directions(Passcode) ->
    <<U:4, D:4, L:4, R:4, _/bitstring>> = crypto:hash(md5, Passcode),
    [Dir || {Dir, Value} <- [{$U, U}, {$D, D}, {$L, L}, {$R, R}], Value > 10].

move(Pos, $U) -> aoc_vector:add(Pos, {0, -1});
move(Pos, $D) -> aoc_vector:add(Pos, {0, 1});
move(Pos, $R) -> aoc_vector:add(Pos, {1, 0});
move(Pos, $L) -> aoc_vector:add(Pos, {-1, 0}).

valid({X, Y}) ->
    X >= 0 andalso X =< 3 andalso Y >= 0 andalso Y =< 3.

neighbours({Dist, Pos, Passcode}) ->
    Ns = [{move(Pos, D), Passcode ++ [D]} || D <- directions(Passcode)],
    [{Dist + 1, P, Pass} || {P, Pass} <- Ns, valid(P)].

bfs([{_, {3, 3}, Passcode} | _]) ->
    Passcode;
bfs([S | Rest]) ->
    bfs(Rest ++ neighbours(S)).

dfs({D, {3, 3}, _}) ->
    D;
dfs(S) ->
    tools:max([dfs(N) || N <- neighbours(S)], 0).
