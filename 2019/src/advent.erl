-module(advent).
-export([run/0, run/1, run/2, run/3]).

file(Name) when is_atom(Name) ->
    file([atom_to_list(Name),".data"]);

file(["../"|_] = Name) ->
    Name;
file(["./"|_] = Name) ->
    Name;
file(["/"|_] = Name) ->
    Name;

file(Name) ->
    Src = filename:dirname(?FILE),
    filename:join([Src, "../data/", Name]).

days() ->
    To = case calendar:universal_time()  of
	     {Date, _} when Date > {2019,12,25} ->
		 25;
	     {{2019,12, Day}, Time} when Time >= {5,0,0} ->
		 Day;
	     {{2019,12, Day}, _} ->
		 Day -1
	 end,
    days(To).

days(To) ->
    [begin
	 String = lists:flatten(["day", integer_to_list(Day)]),
	 list_to_atom(String)
     end || Day <- lists:seq(1,To)].

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
    print([execute(Day, Star, file(File)) || Star <- [star1, star2]]);

run(Day, Star, File) ->
    print(execute(Day, Star, file(File))).

execute(Day) ->
    [execute(Day, Star, file(Day)) || Star <- [star1, star2]].

execute(Day, Star, File) ->
    try timer:tc(Day, run, [Star, File]) of
	{Time, Result} -> 
	    io:format("*~n"),
	    {Day, Star, Time, Result}
    catch
	error:undef -> {Day, undef, 0, not_implemented}
    end.

print({Day, Star, Time, Answer} = Result) ->
    io:format("~-5s | ~-5s | ~8.3f ms | ~p~n", [Day, Star, Time/1000, Answer]),
    Result;

print(List) ->
    io:nl(),
    io:format("Day   | Star  | Time        | Result~n"),
    io:format("------+-------+-------------+-------~n"),
    Results = lists:flatten(List),
    [print(Result) || Result <- Results],
    io:format("------+-------+-------------+-------~n~n"),
    Results.
