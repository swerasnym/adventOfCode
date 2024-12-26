-module(aoc2016_day20).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        {"examples/2016/day20_ex.txt", star1, 3},
        {"examples/2016/day20_ex.txt", star2, 4294967296 - 8}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2016, 20},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Blocked) ->
    smallest(0, Blocked).

star2(Blocked) ->
    count(-1, Blocked, 0).

read(File) ->
    Groups = tools:group(2, tools:read_integers(File, "-\n")),
    lists:sort([order(G) || G <- Groups]).

order({A, B}) when A > B ->
    {B, A};
order(T) ->
    T.

smallest(N, [{Low, _} | _]) when N < Low ->
    N;
smallest(N, [{_, High} | Rest]) ->
    smallest(max(N, High + 1), Rest).

count(N, [], Count) ->
    Count + 4294967296 - N - 1;
count(N, [{Low, High} | Rest], Count) when N < Low ->
    count(High, Rest, Count + Low - N - 1);
count(N, [{_, High} | Rest], Count) ->
    count(max(N, High), Rest, Count).
