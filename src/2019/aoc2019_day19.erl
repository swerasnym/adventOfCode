-module(aoc2019_day19).
-behaviour(aoc_solution).

-export([run/0, run/2]).

%% callbacks
-export([info/0, star1/1, star2/1, read/1]).

info() ->
    Examples = [
        % {"examples/2019/dayN_ex.txt", star1, unknown},
        % {"examples/2019/dayN_ex.txt", star2, unknown}
    ],

    maps:merge(aoc_solution:default_info(), #{
        problem => {2019, 19},
        examples => Examples
    }).

run() ->
    aoc_solution:run(?MODULE).

run(StarOrStars, FileOrData) ->
    aoc_solution:run(?MODULE, StarOrStars, FileOrData).

star1(Program) ->
    Result =
        [
            begin
                Result = intcode:run(Program, [{input, [X, Y]}]),
                intcode:get_output(Result)
            end
         || X <- lists:seq(0, 49), Y <- lists:seq(0, 49)
        ],

    lists:sum(lists:flatten(Result)).

star2(Program) ->
    %Pick any point to the Right of the 100x100 areas
    {X, Y} = find(2000, 1000, Program),
    X * 10000 + Y.

read(File) ->
    intcode:from_file(File).

square(Xi, Yi, Program) ->
    lists:sum(
        lists:flatten([
            begin
                Result = intcode:run(Program, [{input, [X, Y]}]),
                intcode:get_output(Result)
            end
         || X <- [Xi, Xi + 99], Y <- [Yi, Yi + 99]
        ])
    ).

find(X, Y, Program) ->
    case square(X, Y, Program) of
        4 ->
            io:format("~p ", [{X, Y}]),
            find(X, Y, x, Program);
        _ ->
            find(X - 1, Y, Program)
    end.

find(X, Y, x, Program) ->
    case square(X, Y, Program) of
        4 ->
            find(X - 1, Y, x, Program);
        3 ->
            io:format("x ~p~n", [{X + 1, Y}]),
            find3(X + 1, Y, y, Program);
        2 ->
            ok
    end;
find(X, Y, y, Program) ->
    case square(X, Y, Program) of
        4 ->
            find(X, Y - 1, y, Program);
        3 ->
            io:format("y ~p~n", [{X, Y + 1}]),
            find3(X, Y + 1, x, Program);
        2 ->
            ok
    end.

find3(Xi, Yi, Next, Program) ->
    case
        lists:min([
            {X, Y}
         || X <- lists:seq(Xi - 4, Xi),
            Y <- lists:seq(Yi - 4, Yi),
            square(X, Y, Program) == 4
        ])
    of
        {Xi, Yi} ->
            {Xi, Yi};
        {Xx, Yy} ->
            io:format("~p ", [{Xx, Yy}]),
            find(Xx, Yy, Next, Program)
    end.
