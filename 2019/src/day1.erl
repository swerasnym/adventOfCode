-module(day1).
-export([run/2]).

run(Star, File) ->
    {ok, Device} = file:open(File, [read]),
    Data = read_data(Device),
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

star1(Data) ->
    Fuel = lists:map(fun fuel/1, Data),
    lists:sum(Fuel).

star2(Data) ->
    Fuel = lists:map(fun acc_fuel/1, Data),
    lists:sum(Fuel).

read_data(Device) ->
    read_data(Device, []).

read_data(Device, Acc) ->
    case io:fread(Device, [], "~d") of
	eof ->
	    lists:reverse(Acc);
	{ok, [D]} ->
	    read_data(Device, [D | Acc]);
	{error, What} ->
	    io:format("io:fread error: ~w~n", [What]),
	    read_data(Device, Acc)
    end.

acc_fuel(Weight) ->
    acc_fuel(Weight, 0).

acc_fuel(Weight, Acc) ->
    case fuel(Weight) of
	Fuel when Fuel > 0 ->
	    acc_fuel(Fuel, Acc+Fuel);
	_ ->
	    Acc
    end.

fuel(Weight) ->
    Weight div 3 - 2.
