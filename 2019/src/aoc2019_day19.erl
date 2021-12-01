-module(aoc2019_day19).

-export([run/2]).

run(Star, File) ->
    Program = intcode:from_file(File),
    case Star of
        star1 ->
            star1(Program);
        star2 ->
            star2(Program);
        _ ->
            Star1 = star1(Program),
            Star2 = star2(Program),
            {Star1, Star2}
    end.

star1(Program) ->
    Result =
        [begin
             Result = intcode:run(Program, [{input, [X, Y]}]),
             intcode:get_output(Result)
         end
         || X <- lists:seq(0, 49), Y <- lists:seq(0, 49)],

    lists:sum(
        lists:flatten(Result)).

star2(Program) ->
    %Pick any point to the Right of the 100x100 areas
    {X, Y} = find(2000, 1000, Program),
    X * 10000 + Y.

square(Xi, Yi, Program) ->
    lists:sum(
        lists:flatten([begin
                           Result = intcode:run(Program, [{input, [X, Y]}]),
                           intcode:get_output(Result)
                       end
                       || X <- [Xi, Xi + 99], Y <- [Yi, Yi + 99]])).

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
            find(X + 1, Y, y, x, Program);
        2 ->
            ok
    end;
find(X, Y, y, Program) ->
    case square(X, Y, Program) of
        4 ->
            find(X, Y - 1, y, Program);
        3 ->
            io:format("y ~p~n", [{X, Y + 1}]),
            find(X, Y + 1, x, y, Program);
        2 ->
            ok
    end.

find(Xi, Yi, Next, _From, Program) ->
    case lists:min([{X, Y}
                    || X <- lists:seq(Xi - 4, Xi),
                       Y <- lists:seq(Yi - 4, Yi),
                       square(X, Y, Program) == 4])
    of
        {Xi, Yi} ->
            {Xi, Yi};
        {Xx, Yy} ->
            io:format("~p ", [{Xx, Yy}]),
            find(Xx, Yy, Next, Program)
    end.
