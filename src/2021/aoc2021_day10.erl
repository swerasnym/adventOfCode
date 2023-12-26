-module(aoc2021_day10).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"2021/data/dayN_ex.txt", star1, unknown},
        % {"2021/data/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2021, 10},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Data) ->
    Results = [check(D, []) || D <- Data],
    lists:sum([V || {error, V} <- Results]).

star2(Data) ->
    Results = [check(D, []) || D <- Data],
    Incoplete = [I || I <- Results, is_list(I)],
    Scores =
        lists:sort([
            lists:foldl(fun(V, Score) -> Score * 5 + score_i(V) end, 0, Line)
         || Line <- Incoplete
        ]),

    lists:nth(length(Scores) div 2 + 1, Scores).

read(File) ->
    tools:read_lines(File).

check([C | Rest], [C | RestToClose]) ->
    check(Rest, RestToClose);
check([C | Rest], RestToClose) ->
    case close(C) of
        false ->
            {error, score(C)};
        CC ->
            check(Rest, [CC | RestToClose])
    end;
check([], Left) ->
    Left.

close(${) ->
    $};
close($() ->
    $);
close($[) ->
    $];
close($<) ->
    $>;
close(_) ->
    false.

score($}) ->
    1197;
score($)) ->
    3;
score($]) ->
    57;
score($>) ->
    25137.

score_i($}) ->
    3;
score_i($)) ->
    1;
score_i($]) ->
    2;
score_i($>) ->
    4.
