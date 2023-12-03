-module(advent2020).

-export([run/0, run/1, run/2, run/3, average/0, average/1, average/2, average/3, average/4]).

file(Name) when is_atom(Name) ->
    file(atom_to_list(Name) ++ ".data");
file("aoc2020_" ++ Name) ->
    file(Name);
file(["../" | _] = Name) ->
    Name;
file(["./" | _] = Name) ->
    Name;
file(["/" | _] = Name) ->
    Name;
file(Name) ->
    Src = filename:dirname(?FILE),
    filename:join([Src, "../data/", Name]).

days() ->
    To =
        case calendar:universal_time() of
            {Date, _} when Date > {2020, 12, 25} ->
                25;
            {{2020, 12, Day}, Time} when Time >= {5, 0, 0} ->
                Day;
            {{2020, 12, Day}, _} ->
                Day - 1
        end,
    days(To).

days(To) ->
    [
        begin
            String = lists:flatten(["aoc2020_day", integer_to_list(Day)]),
            list_to_atom(String)
        end
     || Day <- lists:seq(1, To)
    ].

run() ->
    Results = [execute(Day) || Day <- days()],
    print(Results).

run(today) ->
    run(lists:last(days()));
run(Day) ->
    run(Day, all).

run(today, Star) ->
    run(lists:last(days()), Star);
run(Day, Star) when is_atom(Star) ->
    run(Day, Star, file(Day));
run(Day, File) ->
    run(Day, all, file(File)).

run(Day, all, File) ->
    print([sep | [execute(Day, Star, file(File)) || Star <- [star1, star2]]]);
run(Day, Star, File) ->
    print([sep, execute(Day, Star, file(File))]).

average() ->
    average(10).

average(Day) when is_atom(Day) ->
    average(10, Day);
average(Times) ->
    Results = [execute_average(Times, Day) || Day <- days()],
    print(Results).

average(Times, today) ->
    average(Times, lists:last(days()));
average(Times, Day) ->
    average(Times, Day, all).

average(Times, today, Star) ->
    average(Times, lists:last(days()), Star);
average(Times, Day, Star) when is_atom(Star) ->
    average(Times, Day, Star, file(Day));
average(Times, Day, File) ->
    average(Times, Day, all, file(File)).

average(Times, Day, all, File) ->
    print([sep | [execute_average(Times, Day, Star, file(File)) || Star <- [star1, star2]]]);
average(Times, Day, Star, File) ->
    print([sep, execute_average(Times, Day, Star, file(File))]).

execute_average(Times, Day) ->
    Res = [execute_average(Times, Day, Star, file(Day)) || Star <- [star1, star2]],
    [sep | Res].

execute_average(Times, Day, Star, File) when Times > 1 ->
    Runs = [execute(Day, Star, File) || _ <- lists:seq(1, Times)],

    {Day, Star, _Time, Result} = hd(Runs),
    RunTimes = [Time || {_Day, _Star, Time, _Result} <- Runs],
    Total = lists:sum(RunTimes),
    {Day, Star, Total div Times, Result}.

parallel_average(Times, Day) ->
    Res = [parallel_average(Times, Day, Star, file(Day)) || Star <- [star1, star2]],
    [sep | Res].

parallel_average(Times, Day, Star, File) ->
    Master = self(),
    Run = fun() -> Master ! {self(), Master, execute(Day, Star, File)} end,

    Pids = [spawn_link(Run) || _ <- lists:seq(1, Times)],

    Runs =
        [
            receive
                {Pid, Master, Result} ->
                    Result
            end
         || Pid <- Pids
        ],

    {Day, Star, _Time, Result} = hd(Runs),
    RunTimes = [Time || {_Day, _Star, Time, _Result} <- Runs],
    Total = lists:sum(RunTimes),
    {Day, Star, Total div Times, Result}.

execute(Day) ->
    Res = [execute(Day, Star, file(Day)) || Star <- [star1, star2]],
    [sep | Res].

execute(Day, Star, File) ->
    flush(),
    io:format("~s:run(~p, ~p).~n", [Day, Star, File]),
    try timer:tc(Day, run, [Star, File]) of
        {Time, Res} ->
            io:format("~s:run(~p, ~p) ->~n    ~p.~n", [Day, Star, File, Res]),
            case Res of
                {Result, _Info} ->
                    {Day, Star, Time, Result};
                Result ->
                    {Day, Star, Time, Result}
            end
    catch
        error:undef:Stack ->
            case hd(Stack) of
                {Day, run, [Star, File], _} ->
                    {Day, Star, 0, {error, undef}};
                _ ->
                    error({undef, Stack})
            end
    end.

print(head) ->
    io:format("Day           | Star  |      Time    | Result~n");
print(sep) ->
    io:format("--------------+-------+--------------+----------------~n");
print({Day, Star, Time, Answer} = Result) ->
    io:format("~-13s | ~-5s | ~9.3f ms | ~p~n", [Day, Star, Time / 1000, Answer]),
    Result;
print(List) ->
    io:nl(),
    print(head),
    Results = lists:flatten(List),
    [print(Result) || Result <- Results],
    print(sep),
    lists:filter(fun(E) -> E /= sep end, Results).

flush() ->
    receive
        _ ->
            flush()
    after 0 ->
        ok
    end.
