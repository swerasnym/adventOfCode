-module(day23).
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
    flush(),
    Table = [{intcode:spawn(Program,[{input, [Address]}, {inputpid, self()}, {outputpid, self()}]), 
	      Address} 
	     || Address <- lists:seq(0,49)],
    switch1(Table).



star2(Program) ->
    flush(),
    Table = [{intcode:spawn(Program,[{input, [Address]}, {inputpid, self()}, {outputpid, self()}]), 
	      Address} 
	     || Address <- lists:seq(0,49)],

    switch2(Table, sets:new(), [none, none], none).

switch1(Table) ->
    receive
	{Pid, input} ->
	    Address = proplists:get_value(Pid, Table),
	    receive
		{save, Address, XY} ->
		    intcode:send(Pid, XY)
	    after 0 ->
		    intcode:send(Pid, [-1])
	    end,
	    switch1(Table);
	



	{Pid, halt}->
	    Address = proplists:get_value(Pid, Table),
	    io:format("~p (~p) halted ~n", [Address, Pid]),
	    
	    switch1(Table);
	

	{Pid, [255]} ->
	    {ok, [_X, Y]} = intcode:recvn(Pid, 2, 1000),
	    [intcode:send(Halt, halt) || Halt <- proplists:get_keys(Table)],
	    Y;
	{Pid, [To]} ->
	    {ok, XY} = intcode:recvn(Pid, 2, 1000),
	    self() ! {save, To, XY},
	    switch1(Table)
		
    after 10000 ->
	    error(timeout)
    end.





switch2(Table, Inactive, [X,Y] = Message, Sent) ->
    case sets:size(Inactive) of
	50 -> %% Idle?

	    case is_idle([], []) of
		active ->
		    io:format("active ~n"),
		    switch2(Table, sets:new(), Message, Sent);
		idle ->
		    io:format("idle ~n"),
		    case Y == Sent of
			true ->
			    io:format("idle2, send: ~p~n", [Message]),
			    [intcode:send(Halt, halt) || Halt <- proplists:get_keys(Table)],
			    Y;
			false ->
			    {Pid, 0} = hd(Table),
			    io:format("idle, send: ~p~n", [Message]),
			    intcode:send(Pid, [X,Y]),
			    switch2(Table, sets:new(), Message, Y)
		    end
	    end;
	_ -> %% not idle
	    receive
		{Pid, input} ->
		    Address = proplists:get_value(Pid, Table),
		    receive
			{save, Address, XY} ->
			    intcode:send(Pid, XY),
			    switch2(Table, Inactive, Message, Sent)
				
		    after 0 ->
			    intcode:send(Pid, [-1]),
			    switch2(Table, sets:add_element(Pid, Inactive), Message, Sent)
		    end;


		{Pid, halt}->
		    Address = proplists:get_value(Pid, Table),
		    io:format("~p (~p) halted ~n", [Address, Pid]),
		    switch2(Table, sets:add_element(Pid, Inactive), Message, Sent);



		{Pid, [255]} ->
		    {ok, XY} = intcode:recvn(Pid, 2, 1000),
		    io:format("nas, recv: ~p~n", [XY]),
		    switch2(Table, sets:del_element(Pid, Inactive), XY, Sent);
		{Pid, [To]} ->
		    {ok, XY} = intcode:recvn(Pid, 2, 1000),
		    self() ! {save, To, XY},
		    
		    {Pid2, To} =  lists:keyfind(To, 2, Table),
		    Inactive1 = sets:del_element(Pid2, Inactive),

		    switch2(Table, sets:del_element(Pid, Inactive1), Message, Sent)

	    after 10000 ->
		    error(timeout)
	    end
    end.

flush() ->
    receive
	_ -> flush()
    after
	0 -> ok
    end.



is_idle(Messages, Input) ->
    receive
	{_Pid, input} = Message ->
	    is_idle(Messages, [Message|Input]);
	Message  -> 
	    is_idle([Message|Messages], Input)
    after
	10 -> %% Fudge factor since all 
	    case Messages of 
		[] -> 
		    [self() ! M0 || M0 <- lists:reverse(Input)],
		    idle;
		Messages ->
		    [self() ! M1 || M1 <- lists:reverse(Messages)], % prioritize send over recive...
		    [self() ! M2 || M2 <- lists:reverse(Input)],
		    active
	    end
    end.
